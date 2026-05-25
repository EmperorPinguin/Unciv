package com.unciv.logic.automation.civilization

import com.unciv.Constants
import com.unciv.logic.city.City
import com.unciv.logic.civilization.Civilization
import com.unciv.logic.civilization.managers.ReligionState
import com.unciv.logic.map.tile.Tile
import com.unciv.models.Counter
import com.unciv.models.Religion
import com.unciv.models.ruleset.Belief
import com.unciv.models.ruleset.BeliefType
import com.unciv.models.ruleset.unique.GameContext
import com.unciv.models.ruleset.unique.UniqueType
import com.unciv.models.stats.Stat
import yairm210.purity.annotations.Readonly
import kotlin.math.min
import kotlin.math.pow
import kotlin.random.Random

object ReligionAutomation {

    // region faith spending

    fun spendFaithOnReligion(civInfo: Civilization) {
        if (civInfo.cities.isEmpty()) return

        // Save for great prophet
        if (civInfo.religionManager.religionState != ReligionState.EnhancedReligion
            && (civInfo.religionManager.remainingFoundableReligions() != 0 || civInfo.religionManager.religionState > ReligionState.Pantheon)
        ) {
            tryBuyGreatProphet(civInfo)
            return
        }
        
        if (civInfo.religionManager.storedFaith >= 100) { // skip check if we likely won't have enough anyways
            // Priorities decided as in poll: https://discord.com/channels/586194543280390151/618491418859798539/1482723902500241470
            if (civInfo.religionManager.religionState == ReligionState.EnhancedReligion || civInfo.religionManager.remainingFoundableReligions() == 0) tryBuyMissionary(civInfo) // save up faith for enhancing as needed
            tryBuyAnyReligiousBuilding(civInfo)
            tryBuyGreatPerson(civInfo)
        }
        return
    }
        
        
    
    private fun tryBuyMissionary(civInfo: Civilization) {
        // Note that in case players complain about AI spreading religion, 
        // it should be examined if this logic needs to be changed or if the player is wrong (many irrationally cherish their own religion)
        // Note that religious victory is not part of Civ5 ruleset, for mod support we should eventually adjust behaviour
        // to prevent a dominant religion from spreading in case this exists as a victory type
        // It's mostly important our cities aren't atheist; in case of foreign religion we'll still get the follower beliefs
        // It may be worth converting cities of foreign religion for the founder benefits etc.,
        // but this needs to be weighed against the faith costs
        // Buy missionaries one at a time; decent chances are cities convert via natural pressure in time,
        // and doing it this way reduces risk of unwanted religion spreading
        
        if (civInfo.units.getCivUnits().any { it.hasUnique(UniqueType.CanSpreadReligion) }) return
        val desiredReligion = getDesiredReligion(civInfo) ?: return
            
        if (civInfo.cities.any { it.religion.getMajorityReligion()?.isMajorReligion() != true }
            // Buy missionaries to spread our religion outside of our civ to civs who want our religion
            //TODO: CS quests
            || (civInfo.gameInfo.civilizations.any { it.isMajorCiv() && it.isAlive() && civInfo.knows(it)
                // TODO: do we need to check if the missionary will be able to reach them?
                && getDesiredReligion(it) == civInfo.religionManager.religion
                && it.cities.none { city -> city.religion.getMajorityReligion() == getDesiredReligion(it) } })) {
            buyMissionaryInAnyCity(civInfo, desiredReligion)
            return
        }
        // Todo: declare war if missionaries enter our civ without permission
    }

    private fun tryBuyAnyReligiousBuilding(civInfo: Civilization) {
        for (city in civInfo.cities) {
            if (city.religion.getMajorityReligion() == null) continue
            val buildings = city.religion.getMajorityReligion()!!.buildingsPurchasableByBeliefs
            val buildingToBePurchased = buildings
                .asSequence()
                .map { civInfo.getEquivalentBuilding(it) }
                .filter { it.isPurchasable(city.cityConstructions) }
                .filter { (it.getStatBuyCost(city, Stat.Faith) ?: return@filter false) <= civInfo.religionManager.storedFaith }
                .minByOrNull { it.getStatBuyCost(city, Stat.Faith)!! }
                ?: continue
            city.cityConstructions.purchaseConstruction(buildingToBePurchased, -1, true, Stat.Faith)
            return
        }
    }

    private fun buyMissionaryInAnyCity(civInfo: Civilization, desiredReligion: Religion) {
        var missionaries = civInfo.gameInfo.ruleset.units.values.filter { unit ->
                unit.hasUnique(UniqueType.CanSpreadReligion)
        }
        missionaries = missionaries.map { civInfo.getEquivalentUnit(it) }

        val missionaryConstruction = missionaries
            // Get list of cities it can be built in
            .associateBy({unit -> unit}) { unit -> civInfo.cities.filter { unit.isPurchasable(it.cityConstructions) && unit.canBePurchasedWithStat(it, Stat.Faith) } }
            .filter { it.value.isNotEmpty() }
            // And from that list determine the cheapest price
            .minByOrNull { it.value.minOf { city -> it.key.getStatBuyCost(city, Stat.Faith)!!  }}?.key
            ?: return


        val hasUniqueToTakeCivReligion = missionaryConstruction.hasUnique(UniqueType.TakeReligionOverBirthCity)

        val validCitiesToBuy = civInfo.cities.filter {
            it.getCenterTile().civilianUnit == null // can't purchase them here
                && (hasUniqueToTakeCivReligion || it.religion.getMajorityReligion() == desiredReligion)
                && (missionaryConstruction.getStatBuyCost(it, Stat.Faith) ?: return@filter false) <= civInfo.religionManager.storedFaith
                && missionaryConstruction.isPurchasable(it.cityConstructions)
                && missionaryConstruction.canBePurchasedWithStat(it, Stat.Faith)
        }
        if (validCitiesToBuy.isEmpty()) return

        val citiesWithBonusCharges = validCitiesToBuy.filter { city ->
            city.getMatchingUniques(UniqueType.UnitStartingPromotions).any {
                val promotionName = it.params[2]
                val promotion = city.getRuleset().unitPromotions[promotionName] ?: return@any false
                promotion.hasUnique(UniqueType.CanSpreadReligion)
            }
        }
        val holyCity = validCitiesToBuy.firstOrNull { it.isHolyCityOf(civInfo.religionManager.religion!!.name) }

        val cityToBuyMissionary = when {
            citiesWithBonusCharges.any() -> citiesWithBonusCharges.first()
            holyCity != null -> holyCity
            else -> validCitiesToBuy.first()
        }

        cityToBuyMissionary.cityConstructions.purchaseConstruction(missionaryConstruction, -1, true, Stat.Faith)
        return
    }

    private fun tryBuyGreatProphet(civInfo: Civilization) {
        if (civInfo.religionManager.religionState < ReligionState.Religion) return
        var greatProphetUnit = civInfo.religionManager.getGreatProphetEquivalent() ?: return
        greatProphetUnit = civInfo.getEquivalentUnit(greatProphetUnit)
        val cityToBuyGreatProphet = civInfo.cities
            .asSequence()
            .filter { greatProphetUnit.isPurchasable(it.cityConstructions) }
            .filter { greatProphetUnit.canBePurchasedWithStat(it, Stat.Faith) }
            .filter { (greatProphetUnit.getStatBuyCost(it, Stat.Faith) ?: return@filter false) <= civInfo.religionManager.storedFaith }
            .minByOrNull { greatProphetUnit.getStatBuyCost(it, Stat.Faith)!! }
            ?: return
        cityToBuyGreatProphet.cityConstructions.purchaseConstruction(greatProphetUnit, -1, true, Stat.Faith)
    }

    private fun tryBuyGreatPerson(civInfo: Civilization) {
        val greatPersonUnit = civInfo.gameInfo.ruleset.units.values.filter {
            it.hasUnique(UniqueType.GreatPerson) && !it.hasUnique(UniqueType.MayFoundReligion) //we want to exclude great prophets from the list
        }
        val greatPersonConstruction = greatPersonUnit
            // Get list of cities it can be built in
            .associateBy({unit -> unit}) { unit -> civInfo.cities.filter { unit.isPurchasable(it.cityConstructions) && unit.canBePurchasedWithStat(it, Stat.Faith) } }
            .filter { it.value.isNotEmpty() }
            // And from that list determine the cheapest price
            .minByOrNull { it.value.minOf { city -> it.key.getStatBuyCost(city, Stat.Faith)!!  }}?.key
            ?: return

        val validCitiesToBuy = civInfo.cities.filter {
            (greatPersonConstruction.getStatBuyCost(it, Stat.Faith) ?: return@filter false) <= civInfo.religionManager.storedFaith
        }
        
        if (validCitiesToBuy.isEmpty()) return

        val cityToBuy = validCitiesToBuy.first()

        cityToBuy.cityConstructions.purchaseConstruction(greatPersonConstruction, -1, true, Stat.Faith)
    }

    // endregion

    // region rate beliefs
    @Readonly
    fun getDesiredReligion(civInfo: Civilization): Religion? {
        val desiredReligion = if (civInfo.religionManager.remainingFoundableReligions() != 0 
                || civInfo.religionManager.religionState >= ReligionState.Religion) {
            civInfo.religionManager.religion
        } else {
            civInfo.gameInfo.religions.values.maxByOrNull { rateReligion(civInfo, it) }
        }
        return desiredReligion
    }
    
    @Readonly
    private fun rateReligion(civInfo: Civilization, religion: Religion): Float {
        var rating = 0f
        for (belief in religion.getAllBeliefsOrdered())
            rating += rateBelief(civInfo, belief, false)
        return  rating
    }
    
    @Readonly
    fun rateBelief(civInfo: Civilization, belief: Belief, countPlayerBeliefs: Boolean = true): Float {
        var score = 0f // Roughly equivalent to the sum of stats gained across all cities

        for (city in civInfo.cities) {
            for (tile in city.getCenterTile().getTilesInDistance(city.getWorkRange())) {
                val tileScore = beliefBonusForTile(belief, tile, city)
                score += tileScore * when {
                    city.workedTiles.contains(tile.position) -> 1f // worked
                    tile.getCity() == city -> 0.5f // workable, but probably bad or we'd be working it
                    //tile.isCoastalTile() -> 0.8f // it's 'nicer' to go for less-contested pantheons, and in various rulesets seems to have good synergy
                    else -> 0.6f // unavailable - for now
                } 
            }

            score += beliefBonusForCity(civInfo, belief, city)
        }

        if (countPlayerBeliefs) score += beliefBonusForPlayer(civInfo, belief)

        // there is enough map RNG for tile-based beliefs. Let's add Personality for city- and player-based beliefs instead of RNG
        
        score *= belief.getWeightForAiDecision(GameContext(civInfo))

        return score
    }

    @Readonly
    private fun beliefBonusForTile(belief: Belief, tile: Tile, city: City): Float {
        var bonusYield = 0f
        for (unique in belief.uniqueObjects) {
            when (unique.type) {
                UniqueType.StatsFromObject -> {
                    val resource = tile.tileResource
                    if (tile.matchesFilter(unique.params[1])) {
                        if (!tile.lastTerrain.hasUnique(UniqueType.ProductionBonusWhenRemoved) ||
                            !tile.lastTerrain.matchesFilter(unique.params[1]) //forest pantheons are bad, as we want to remove the forests
                        ) bonusYield += unique.stats.sum()
                        else if (resource != null && (resource.matchesFilter(unique.params[1]) ||
                                resource.isImprovedBy(unique.params[1]))
                        ) bonusYield += unique.stats.sum() //resource pantheons are good, as we want to work the tile anyways
                    } else if (resource != null && (resource.matchesFilter(unique.params[1]) ||
                            resource.isImprovedBy(unique.params[1]))
                    ) bonusYield += unique.stats.sum() //resource pantheons are good, as we want to work the tile anyways
                }
                UniqueType.StatsFromTilesWithout -> {
                    if (city.matchesFilter(unique.params[3]) &&
                        tile.matchesFilter(unique.params[1]) &&
                        !tile.matchesFilter(unique.params[2])
                    ) bonusYield += unique.stats.sum()
                }
                else -> {}
            }
        }
        return bonusYield
    }

    @Readonly
    private fun beliefBonusForCity(civInfo: Civilization, belief: Belief, city: City): Float {
        var score = 0f
        val ruleSet = civInfo.gameInfo.ruleset
        for (unique in belief.uniqueObjects) {
            val modifier = if (unique.getModifiers(UniqueType.ConditionalNotWar).isEmpty()) 1f else 0f // can be cancelled by just declaring war
            // todo: rank the other conditionals property (e.g. minimum pop requirement)
            score += modifier * when (unique.type) {
                UniqueType.GrowthPercentBonus -> unique.params[0].toFloat() / 3f
                UniqueType.BorderGrowthPercentage -> -unique.params[0].toFloat() / 15f
                UniqueType.StrengthForCities -> unique.params[0].toFloat() / 20f // Modified by personality
                UniqueType.CityHealingUnits -> unique.params[1].toFloat() / 60f // AI doesn't relly know how to use this
                UniqueType.PercentProductionBuildings -> unique.params[0].toFloat() / 5f
                UniqueType.PercentProductionWonders -> unique.params[0].toFloat() / 15f
                UniqueType.PercentProductionUnits -> unique.params[0].toFloat() / 5f
                UniqueType.StatsFromCitiesOnSpecificTiles ->
                    if (city.getCenterTile().matchesFilter(unique.params[1]))
                        unique.stats.sum()// Modified by personality
                    else 0f
                UniqueType.StatsFromObject ->
                    when {
                        ruleSet.buildings.containsKey(unique.params[1]) -> {
                            unique.stats.sum() *
                                if (ruleSet.buildings[unique.params[1]]!!.isNationalWonder) 0.25f //there's at most 1 copy of each of these in our empire, and the AI is slow at getting it
                                else 1f // Yields from regular buildings won't need the upfront purchase cost as is the case with religion buildings, but they may have weird requirements (gardens etc.)

                        }
                        ruleSet.specialists.containsKey(unique.params[1]) -> {
                            unique.stats.sum() *
                                if (city.population.population > 8f) 1.5f // choose this if we're pretty late already, otherwise there are probably faster benefits
                                else 0f
                        }
                        else -> unique.stats.sum() * 0f //yields from world wonders and great improvements - the latter needs additional AI logic to be used correctly
                    }
                UniqueType.StatsFromTradeRoute ->
                    unique.stats.sum() *
                        if (city.isConnectedToCapital()) 1f
                        else 0f //no yields from the belief yet, also for pantheons it's quite low-tempo
                UniqueType.StatPercentFromReligionFollowers ->
                    min(unique.params[0].toFloat() * city.population.population, unique.params[2].toFloat())
                UniqueType.StatsPerCity ->
                    if (city.matchesFilter(unique.params[1]))
                        unique.stats.sum() * 2f //free and immediate yields, let's pick it first
                    else 0f
                else -> 0f
            }
        }

        return score
    }

    @Readonly
    private fun beliefBonusForPlayer(civInfo: Civilization, belief: Belief): Float {
        var score = 0f
        val numberOfFoundedReligions = civInfo.gameInfo.civilizations.count {
            it.religionManager.religion != null && it.religionManager.religionState >= ReligionState.Religion
        }
        val maxNumberOfReligions = numberOfFoundedReligions + civInfo.religionManager.remainingFoundableReligions()

        // adjusts scores of certain beliefs as game evolves (adapted from Civ 5 DLL files on AI belief selection)
        // enable differentiation of early vs late founding of religion and early vs late enhancement of religion
        // this is mainly for mods which may shuffle enhancer and founder beliefs w.r.t. base Unciv
        var gameTimeScalingPercent = 100
        when (civInfo.religionManager.religionState) {
            ReligionState.FoundingReligion -> {
                gameTimeScalingPercent = 100 - ((numberOfFoundedReligions * 100) / maxNumberOfReligions)
            }
            ReligionState.EnhancingReligion -> {
                val amountOfEnhancedReligions = civInfo.gameInfo.civilizations.count {
                    it.religionManager.religion != null && it.religionManager.religionState == ReligionState.EnhancedReligion
                }
                gameTimeScalingPercent = 100 - ((amountOfEnhancedReligions * 100) / maxNumberOfReligions)
            }
            else -> {} // pantheon shouldn't matter
        }
        val goodEarlyModifier = when {
            gameTimeScalingPercent < 33 -> 1f
            gameTimeScalingPercent < 66 -> 2f
            else -> 4f
        }
        val goodLateModifier = when {
            gameTimeScalingPercent < 33 -> 2f
            gameTimeScalingPercent < 66 -> 1f
            else -> 1/2f
        }

        for (unique in belief.uniqueObjects) {
            val modifier =
                if (unique.getModifiers(UniqueType.ConditionalOurUnit).any { it.params[0] == civInfo.religionManager.getGreatProphetEquivalent()?.name }) 1/2f
                else 1f
            // Some city-filters are modified by personality (non-enemy foreign cities)
            score += modifier * when (unique.type) {
                UniqueType.KillUnitPlunderNearCity -> 0f //can be very strong, but the AI currently isn't farming barb camps
                UniqueType.BuyUnitsForAmountStat, UniqueType.BuyBuildingsForAmountStat ->
                    if (civInfo.religionManager.religion != null
                        && civInfo.religionManager.religion!!.followerBeliefUniqueMap.getUniques(unique.type).any()
                    ) 0f
                    // This is something completely different from the original, but I have no idea
                    // what happens over there
                    else civInfo.stats.statsForNextTurn[Stat.valueOf(unique.params[2])] * 300f / unique.params[1].toFloat() //the costs of these are probably similar to the baseUnitBuyCost
                UniqueType.BuyUnitsWithStat, UniqueType.BuyBuildingsWithStat ->
                    if (civInfo.religionManager.religion != null
                        && civInfo.religionManager.religion!!.followerBeliefUniqueMap.getUniques(unique.type).any()
                    ) 0f
                    // This is something completely different from the original, but I have no idea
                    // what happens over there
                    else civInfo.stats.statsForNextTurn[Stat.valueOf(unique.params[1])] * 300f / civInfo.getEra().baseUnitBuyCost
                    //baseUnitBuyCost is 200 in Standard speed pre-Renaissance, but overvalue such as to let the high-faith civs pick the good faith sinks 
                UniqueType.BuyUnitsByProductionCost ->
                    0f //Holy Warriors is a waste if we don't buy units with it, and if we buy units with it'll cost us great persons
                UniqueType.StatsWhenSpreading ->
                    unique.params[0].toFloat() / 40f
                UniqueType.StatsWhenAdoptingReligion ->
                    unique.stats.sum() / 80f
                UniqueType.RestingPointOfCityStatesFollowingReligionChange ->
                    unique.params[0].toFloat() / 10f
                UniqueType.StatsFromGlobalCitiesFollowingReligion ->
                    unique.stats.sum() //free yields that are potentially more than our own number of cities would allow
                UniqueType.StatsFromGlobalFollowers ->
                    if (unique.params[2] == "in non-enemy foreign cities") 0f // don't adopt beliefs we can't afford others to cancel
                    else if (unique.params[2] == "in foreign cities") 8f * (unique.stats.sum() / unique.params[1].toFloat()) // worse than the next one, although the AI always aims to spread their religion so maybe we should switch it up
                    else  10f * (unique.stats.sum() / unique.params[1].toFloat()) 
                UniqueType.Strength ->
                    unique.params[0].toFloat() //combat strength from beliefs is very strong
                UniqueType.ReligionSpreadDistance ->
                    unique.params[0].toFloat() * goodEarlyModifier
                UniqueType.NaturalReligionSpreadStrength ->
                    if (unique.params[1] == "in City-State cities") 0f else unique.params[0].toFloat() * goodEarlyModifier / 20f
                UniqueType.SpreadReligionStrength ->
                    unique.params[0].toFloat() * goodLateModifier / 20f //Cheaper missionaries are generally better than stronger missionaries
                UniqueType.FaithCostOfGreatProphetChange ->
                    -unique.params[0].toFloat() * goodLateModifier / 25f //It's only about 1 more prophet, due to the increasing costs.
                UniqueType.BuyBuildingsDiscount, UniqueType.BuyUnitsDiscount ->
                    -unique.params[2].toFloat() * goodLateModifier / 10f
                UniqueType.BuyItemsDiscount ->
                    -unique.params[1].toFloat() * goodLateModifier / 10f
                else -> 0f
            }
        }

        return score
    }


    internal fun chooseReligiousBeliefs(civInfo: Civilization) {
        choosePantheon(civInfo)
        foundReligion(civInfo)
        enhanceReligion(civInfo)
        chooseFreeBeliefs(civInfo)
    }

    private fun choosePantheon(civInfo: Civilization) {
        if (!civInfo.religionManager.canFoundOrExpandPantheon()) return
        // So looking through the source code of the base game available online,
        // the functions for choosing beliefs total in at around 400 lines.
        // https://github.com/Gedemon/Civ5-DLL/blob/aa29e80751f541ae04858b6d2a2c7dcca454201e/CvGameCoreDLL_Expansion1/CvReligionClasses.cpp
        // line 4426 through 4870.
        // This is way too much work for now, so I'll just choose a random pantheon instead.
        // Should probably be changed later, but it works for now.
        val chosenPantheon = pickBeliefOfType(civInfo, BeliefType.Pantheon)
            ?: return // panic!
        civInfo.religionManager.chooseBeliefs(
            listOf(chosenPantheon),
            useFreeBeliefs = civInfo.religionManager.usingFreeBeliefs()
        )
    }

    private fun foundReligion(civInfo: Civilization) {
        if (civInfo.religionManager.religionState != ReligionState.FoundingReligion) return
        val usedReligions = civInfo.gameInfo.religions.values.mapTo(mutableSetOf()) { it.name }
        val availableReligions = civInfo.gameInfo.ruleset.religions.filterNot { it in usedReligions }
        val favoredReligion = civInfo.nation.favoredReligion?.takeIf { it in availableReligions }
        val allFavoredReligions = civInfo.gameInfo.civilizations.mapNotNullTo(mutableSetOf()) { it.nation.favoredReligion}
        val nonFavoredReligions = availableReligions.filterNot { it in allFavoredReligions }
        val chosenReligion = favoredReligion
            ?: nonFavoredReligions.randomOrNull() // allow other civs to found their own favoured religion when possible
            ?: availableReligions.randomOrNull()
            ?: return // Wait what? How did we pass the checking when using a great prophet but not this?

        civInfo.religionManager.foundReligion(chosenReligion, chosenReligion)

        val chosenBeliefs = chooseBeliefs(civInfo, civInfo.religionManager.getBeliefsToChooseAtFounding()).toList()
        civInfo.religionManager.chooseBeliefs(chosenBeliefs)
    }

    private fun enhanceReligion(civInfo: Civilization) {
        if (civInfo.religionManager.religionState != ReligionState.EnhancingReligion) return
        civInfo.religionManager.chooseBeliefs(
            chooseBeliefs(civInfo, civInfo.religionManager.getBeliefsToChooseAtEnhancing()).toList()
        )
    }

    private fun chooseFreeBeliefs(civInfo: Civilization) {
        if (!civInfo.religionManager.hasFreeBeliefs()) return
        civInfo.religionManager.chooseBeliefs(
            chooseBeliefs(civInfo, civInfo.religionManager.freeBeliefsAsEnums()).toList(),
            useFreeBeliefs = true
        )
    }

    private fun chooseBeliefs(civInfo: Civilization, beliefsToChoose: Counter<BeliefType>): HashSet<Belief> {
        val chosenBeliefs = hashSetOf<Belief>()
        // The `continue`s should never be reached, but just in case I'd rather have the AI have a
        // belief less than make the game crash. The `continue`s should only be reached whenever
        // there are not enough beliefs to choose, but there should be, as otherwise we could
        // not have used a great prophet to found/enhance our religion.
        for (belief in BeliefType.entries) {
            if (belief == BeliefType.None) continue
            repeat(beliefsToChoose[belief]) {
                chosenBeliefs.add(
                    pickBeliefOfType(civInfo, belief, chosenBeliefs) ?: return@repeat
                )
            }
        }
        return chosenBeliefs
    }

    @Readonly
    private fun pickBeliefOfType(civInfo: Civilization, beliefType: BeliefType, additionalBeliefsToExclude: HashSet<Belief> = hashSetOf()): Belief? {
        return civInfo.gameInfo.ruleset.beliefs.values
            .filter {
                (it.type == beliefType || beliefType == BeliefType.Any)
                    && !additionalBeliefsToExclude.contains(it)
                    && civInfo.religionManager.getReligionWithBelief(it) == null
                    && it.getMatchingUniques(UniqueType.OnlyAvailable, GameContext.IgnoreConditionals)
                    .none { unique -> !unique.conditionalsApply(civInfo.state) }
            }
            .maxByOrNull { rateBelief(civInfo, it) }
    }


    //endregion
}

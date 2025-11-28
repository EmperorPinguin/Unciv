package com.unciv.logic.automation.unit

import com.unciv.Constants
import com.unciv.logic.battle.AttackableTile
import com.unciv.logic.battle.Battle
import com.unciv.logic.battle.BattleDamage
import com.unciv.logic.battle.CityCombatant
import com.unciv.logic.battle.MapUnitCombatant
import com.unciv.logic.battle.TargetHelper
import com.unciv.logic.city.City
import com.unciv.logic.map.mapunit.MapUnit
import com.unciv.models.ruleset.unique.GameContext
import com.unciv.models.ruleset.unique.UniqueType
import com.unciv.ui.screens.victoryscreen.RankingType
import yairm210.purity.annotations.Readonly
import kotlin.math.abs
import kotlin.math.roundToInt

object BattleHelper {

    /** Returns true if the unit cannot further move this turn - NOT if an attack was successful! */
    fun tryAttackNearbyEnemy(unit: MapUnit, stayOnTile: Boolean = false, cautionLevel: Float = 0f): Boolean {
        if (unit.civ.civName == Constants.simulationCiv2)
            return tryAttackNearbyEnemyNew(unit, stayOnTile, cautionLevel)
        
        if (unit.hasUnique(UniqueType.CannotAttack)) return false
        val distanceToTiles = unit.movement.getDistanceToTiles()
        val attackableEnemies = TargetHelper.getAttackableEnemies(unit, unit.movement.getDistanceToTiles(), stayOnTile=stayOnTile)
            // Only take enemies we can fight without dying or are made to die
            .filter {
                val defender = Battle.getMapCombatantOfTile(it.tileToAttack)
                unit.hasUnique(UniqueType.SelfDestructs) || (defender != null &&
                (BattleDamage.calculateDamageToAttacker(
                    MapUnitCombatant(unit),
                    defender) < unit.health
                    && unit.getDamageFromTerrain(it.tileToAttackFrom) <= 0))
                    // For mounted units it is fine to attack from these tiles, but with current AI movement logic it is not easy to determine if our unit can meaningfully move away after attacking
                    // Also, AI doesn't build tactical roads
            }

        val enemyTileToAttack = chooseAttackTarget(unit, attackableEnemies)

        if (enemyTileToAttack != null) {
            if (enemyTileToAttack.tileToAttack.militaryUnit == null && unit.baseUnit.isRanged()
                && unit.movement.canMoveTo(enemyTileToAttack.tileToAttack)
                && distanceToTiles.containsKey(enemyTileToAttack.tileToAttack)) { // Since the 'getAttackableEnemies' could return a tile we attack at range but cannot reach
                // Ranged units should move to caputre a civilian unit instead of attacking it
                unit.movement.moveToTile(enemyTileToAttack.tileToAttack)
            } else {
                Battle.moveAndAttack(MapUnitCombatant(unit), enemyTileToAttack)
            }
        }
        return !unit.hasMovement()
    }

    /**
     * Returns true if the unit cannot further move this turn - NOT if an attack was successful!
     * @param cautionLevel 0f = normal behavior, 1f = only attack if we benefit greatly, such as by killing a unit or capturing a city
     */
    fun tryAttackNearbyEnemyNew(unit: MapUnit, stayOnTile: Boolean = false, cautionLevel: Float = 0f): Boolean {
        if (unit.hasUnique(UniqueType.CannotAttack)) return false
        val distanceToTiles = unit.movement.getDistanceToTiles()
        val attackableEnemies = TargetHelper.getAttackableEnemies(unit, distanceToTiles, stayOnTile=stayOnTile)
            // Only take enemies we can fight without dying or are made to die
            .filter {
                val defender = Battle.getMapCombatantOfTile(it.tileToAttack)
                unit.hasUnique(UniqueType.SelfDestructs) || (defender != null &&
                    (BattleDamage.calculateDamageToAttacker(
                        MapUnitCombatant(unit),
                        defender) < unit.health
                        && unit.getDamageFromTerrain(it.tileToAttackFrom) <= 0))
                // For mounted units it is fine to attack from these tiles, but with current AI movement logic it is not easy to determine if our unit can meaningfully move away after attacking
                // Also, AI doesn't build tactical roads
            }

        // attack even if we lose more HP than our opponent
        val minAttackValueCaution0f = -50
        // only attack if we can kill a unit or capture a valuable civilian
        val minAttackValueCaution1f = 125
        val minAttackValue: Int = minAttackValueCaution0f + (cautionLevel * (minAttackValueCaution1f - minAttackValueCaution0f)).roundToInt()
        val enemyTileToAttack = chooseAttackTarget(unit, attackableEnemies, minAttackValue)

        if (enemyTileToAttack != null) {
            if (enemyTileToAttack.tileToAttack.militaryUnit == null && unit.baseUnit.isRanged()
                && unit.movement.canMoveTo(enemyTileToAttack.tileToAttack)
                && distanceToTiles.containsKey(enemyTileToAttack.tileToAttack)) { // Since the 'getAttackableEnemies' could return a tile we attack at range but cannot reach
                // Ranged units should move to caputre a civilian unit instead of attacking it
                unit.movement.moveToTile(enemyTileToAttack.tileToAttack)
            } else {
                Battle.moveAndAttack(MapUnitCombatant(unit), enemyTileToAttack)
            }
        }
        return !unit.hasMovement()
    }

    fun tryDisembarkUnitToAttackPosition(unit: MapUnit): Boolean {
        if (!unit.baseUnit.isMelee() || !unit.baseUnit.isLandUnit || !unit.isEmbarked()) return false
        val unitDistanceToTiles = unit.movement.getDistanceToTiles()

        val attackableEnemiesNextTurn = TargetHelper.getAttackableEnemies(unit, unitDistanceToTiles)
                // Only take enemies we can fight without dying
                .filter {
                    BattleDamage.calculateDamageToAttacker(
                        MapUnitCombatant(unit),
                        Battle.getMapCombatantOfTile(it.tileToAttack)!!
                    ) < unit.health
                }
                .filter { it.tileToAttackFrom.isLand }

        val enemyTileToAttackNextTurn = chooseAttackTarget(unit, attackableEnemiesNextTurn)

        if (enemyTileToAttackNextTurn != null) {
            unit.movement.moveToTile(enemyTileToAttackNextTurn.tileToAttackFrom)
            return true
        }
        return false
    }

    /**
     * Choses the best target in attackableEnemies, this could be a city or a unit.
     * @param minAttackValue Do not consider enemies below this attack value
     */
    @Readonly
    private fun chooseAttackTarget(unit: MapUnit, attackableEnemies: List<AttackableTile>, minAttackValue: Int = -25): AttackableTile? {
        if (unit.civ.civName == Constants.simulationCiv2)
            return chooseAttackTargetNew(unit, attackableEnemies, minAttackValue)
        
        // Get the highest valued attackableEnemy
        var highestAttackValue = 0
        var attackTile: AttackableTile? = null
        // We always have to calculate the attack value even if there is only one attackableEnemy
        for (attackableEnemy in attackableEnemies) {
            val tempAttackValue = if (attackableEnemy.tileToAttack.isCityCenter())
                getCityAttackValue(unit, attackableEnemy.tileToAttack.getCity()!!)
            else getUnitAttackValue(unit, attackableEnemy)
            if (tempAttackValue > highestAttackValue) {
                highestAttackValue = tempAttackValue
                attackTile = attackableEnemy
            }
        }
        // todo For air units, prefer to attack tiles with lower intercept chance
        // Only return that tile if it is actually a good tile to attack
        return if (highestAttackValue > 30) attackTile else null
    }

    /**
     * Choses the best target in attackableEnemies, this could be a city or a unit.
     * @param minAttackValue Do not consider enemies below this attack value
     */
    @Readonly
    private fun chooseAttackTargetNew(unit: MapUnit, attackableEnemies: List<AttackableTile>, minAttackValue: Int = -25): AttackableTile? {
        // Get the highest valued attackableEnemy
        var highestAttackValue = Int.MIN_VALUE
        var attackTile: AttackableTile? = null
        // We always have to calculate the attack value even if there is only one attackableEnemy
        for (attackableEnemy in attackableEnemies) {
            val enemy = attackableEnemy.combatant!!

            //print("${unit.civ.civName}'s ${unit.name} (${unit.health}) considers attacking ")
            val tempAttackValue = if (attackableEnemy.tileToAttack.isCityCenter())
                getCityAttackValue(unit, attackableEnemy.tileToAttack.getCity()!!)
            else getUnitAttackValue(unit, attackableEnemy)
            /*println("%s's %s (%d) considers attacking %s's %s (%d) with value %d".format(
                unit.civ.civName,
                unit.name,
                unit.health,
                enemy.getCivInfo().civName,
                enemy.getName(),
                enemy.getHealth(),
                tempAttackValue
            ))*/
            //println(tempAttackValue)
            if (tempAttackValue > highestAttackValue) {
                highestAttackValue = tempAttackValue
                attackTile = attackableEnemy
            }
        }
        //return if (highestAttackValue > 30) attackTile else null
        if (highestAttackValue >= minAttackValue && attackTile!!.combatant != null) {
            val enemy = attackTile.combatant
            /*println("%s's %s (%d) attacking %s's %s (%d) with value %d".format(
                unit.civ.civName,
                unit.name,
                unit.health,
                enemy.getCivInfo().civName,
                enemy.getName(),
                enemy.getHealth(),
                highestAttackValue
            ))*/
        }
        // todo For air units, prefer to attack tiles with lower intercept chance
        // Only return that tile if it is actually a good tile to attack
        return if (highestAttackValue >= minAttackValue) attackTile else null
    }

    /**
     * Returns a value which represents the attacker's motivation to attack a city.
     * Siege units will almost always attack cities.
     * Base value is 100(Mele) 110(Ranged) standard deviation is around 80 to 130
     */
    @Readonly
    private fun getCityAttackValue(attacker: MapUnit, city: City): Int {
        if (attacker.civ.civName == Constants.simulationCiv2)
            return getCityAttackValueNew(attacker, city)
        
        val attackerUnit = MapUnitCombatant(attacker)
        val cityUnit = CityCombatant(city)
        
        val canCaptureCity = attacker.baseUnit.isMelee() && !attacker.hasUnique(UniqueType.CannotCaptureCities)
        if (city.health == 1)
            return if (canCaptureCity) 10000 // Capture the city immediately!
            else 0 // No reason to attack, we won't make any difference
        
        if (canCaptureCity && city.health <= BattleDamage.calculateDamageToDefender(attackerUnit, cityUnit).coerceAtLeast(1))
            return 10000
            

        if (attacker.baseUnit.isMelee()) {
            val battleDamage = BattleDamage.calculateDamageToAttacker(attackerUnit, cityUnit)
            if (attacker.health - battleDamage * 2 <= 0 && !attacker.hasUnique(UniqueType.SelfDestructs)) {
                // The more fiendly units around the city, the more willing we should be to just attack the city
                val friendlyUnitsAroundCity = city.getCenterTile().getTilesInDistance(3).count { it.militaryUnit?.civ == attacker.civ }
                // If we have more than 4 other units around the city, go for it
                if (friendlyUnitsAroundCity < 5) {
                    val attackerHealthModifier = 1.0 + 1.0 / friendlyUnitsAroundCity
                    if (attacker.health - battleDamage * attackerHealthModifier <= 0)
                        return 0 // We'll probably die next turn if we attack the city
                }
            }
        }

        var attackValue = 100
        // Siege units should really only attack the city
        if (attacker.baseUnit.isProbablySiegeUnit()) attackValue += 100
        // Ranged units don't take damage from the city
        else if (attacker.baseUnit.isRanged()) attackValue += 10
        // Lower health cities have a higher priority to attack ranges from -20 to 30
        attackValue -= (city.health - 60) / 2

        // Add value based on number of units around the city
        val defendingCityCiv = city.civ
        city.getCenterTile().getTilesInDistance(2).forEach {
            if (it.militaryUnit != null) {
                if (it.militaryUnit!!.civ.isAtWarWith(attacker.civ))
                    attackValue -= 5
                if (it.militaryUnit!!.civ.isAtWarWith(defendingCityCiv))
                    attackValue += 15
            }
        }

        return attackValue
    }
    
    /**
     * Returns a value which represents the attacker's motivation to attack a city.
     * Siege units will almost always attack cities.
     * Base value is 100(Mele) 110(Ranged) standard deviation is around 80 to 130
     */
    @Readonly
    private fun getCityAttackValueNew(attacker: MapUnit, city: City): Int {
        val attackerUnit = MapUnitCombatant(attacker)
        val cityUnit = CityCombatant(city)

        val canCaptureCity = attacker.baseUnit.isMelee() && !attacker.hasUnique(UniqueType.CannotCaptureCities)
        if (city.health == 1)
            return if (canCaptureCity) 10000 // Capture the city immediately!
            else -10000 // No reason to attack, we won't make any difference

        if (canCaptureCity && city.health <= BattleDamage.calculateDamageToDefender(attackerUnit, cityUnit).coerceAtLeast(1))
            return 10000


        if (attacker.baseUnit.isMelee()) {
            val battleDamage = BattleDamage.calculateDamageToAttacker(attackerUnit, cityUnit)
            if (attacker.health - battleDamage * 2 <= 0 && !attacker.hasUnique(UniqueType.SelfDestructs)) {
                // The more fiendly units around the city, the more willing we should be to just attack the city
                val friendlyUnitsAroundCity = city.getCenterTile().getTilesInDistance(3).count { it.militaryUnit?.civ == attacker.civ }
                // If we have more than 4 other units around the city, go for it
                if (friendlyUnitsAroundCity < 5) {
                    val attackerHealthModifier = 1.0 + 1.0 / friendlyUnitsAroundCity
                    if (attacker.health - battleDamage * attackerHealthModifier <= 0)
                        return -10000 // We'll probably die next turn if we attack the city
                }
            }
        }

        var attackValue = 0
        // Siege units should really only attack the city
        if (attacker.baseUnit.isProbablySiegeUnit()) attackValue += 150
        // Ranged units don't take damage from the city
        else if (attacker.baseUnit.isRanged()) attackValue += 50
        // Lower health cities have a higher priority to attack ranges from -20 to 30
        attackValue -= (city.health - 60) / 2

        // Add value based on number of units around the city
        val defendingCityCiv = city.civ
        city.getCenterTile().getTilesInDistance(2).forEach {
            if (it.militaryUnit != null) {
                // nearby enemies
                if (it.militaryUnit!!.civ.isAtWarWith(attacker.civ))
                    attackValue -= 5
                // nearby allies
                if (it.militaryUnit!!.civ.isAtWarWith(defendingCityCiv))
                    attackValue += 15
            }
        }

        return attackValue
    }
    
    // includes ranged unit moving to capture or destroy civilian
    @Readonly
    private fun getUnitAttackValueNew(attacker: MapUnit, attackTile: AttackableTile): Int {
        var attackValue = 0

        val militaryUnit = attackTile.tileToAttack.militaryUnit
        var canKillEscort = false
        // scout vs GDR -> -250
        // GDR vs scout -> +200
        // scout vs scout -> 0
        if (militaryUnit != null) {
            val attackerForce = attacker.civ.getStatForRanking(RankingType.Force)
            val defenderForce = militaryUnit.civ.getStatForRanking(RankingType.Force)
            val forceRatio = (attackerForce.toFloat() / defenderForce).coerceIn(0.5f, 2f)
            // bold if we are strong, cautious if we are weak
            if (forceRatio >= 1f)
                attackValue += (25 * forceRatio).roundToInt() - 25
            else
                attackValue -= (25 / forceRatio).roundToInt() - 25
            
            val damageToAttacker = BattleDamage.calculateDamageToAttacker(MapUnitCombatant(attacker), MapUnitCombatant(militaryUnit))
            val attackerPercentageRemainingHpLost = 100 * damageToAttacker / attacker.health
            // losing HP is bad
            attackValue -= damageToAttacker.coerceAtMost(100) / 2
            // losing HP is especially bad if we have low HP
            attackValue -= attackerPercentageRemainingHpLost.coerceAtMost(100) / 2
            // suicide is very bad
            if (attackerPercentageRemainingHpLost >= 100)
                attackValue -= 150
            
            val damageToDefender = BattleDamage.calculateDamageToDefender(MapUnitCombatant(attacker), MapUnitCombatant(militaryUnit))
            val defenderPercentageRemainingHpLost = 100 * damageToDefender / militaryUnit.health
            // dealing damage is good
            attackValue += damageToDefender.coerceAtMost(100) / 2
            // dealing damage is especially good if they have low HP
            attackValue += defenderPercentageRemainingHpLost.coerceAtMost(100) / 2
            // killing is very good - mom, it's just a video game
            if (defenderPercentageRemainingHpLost >= 100)
                attackValue += 100
            
            // the unit is not necessarily an escort
            canKillEscort = defenderPercentageRemainingHpLost >= 100

            // todo factor in force value
        }
        
        // todo ranged units should value killing an escort slightly higher than non-escort military units
        
        // kill GP -> +150 - +225
        // capture worker -> +30 - +45
        // kill worker -> -30 - -45
        val civilianUnit = attackTile.tileToAttack.civilianUnit
        
        // skip if the attacker is ranged and already attacked the escort, because it can't touch the civilian
        if (civilianUnit != null && (militaryUnit == null || (attacker.baseUnit.isMelee() && canKillEscort))) {
            
            // value per point of damage dealt, divided by 100
            fun damageValue(
                damage: Int //= BattleConstants.DAMAGE_TO_CIVILIAN_UNIT.coerceAtMost(civilianUnit.health)
            ) = damage * when {
                // GP
                civilianUnit.isGreatPerson() -> 75
                // settler
                civilianUnit.hasUnique(UniqueType.FoundCity, GameContext.IgnoreConditionals) -> 30
                // uncapturable civilian
                civilianUnit.hasUnique(UniqueType.Uncapturable) -> 10
                // capturable civilian
                else -> -25
            } / 100
            
            // if we can one-hit-kill multiple units, choose the one with highest HP
            fun killValue() = damageValue(200) + damageValue(civilianUnit.health)

            // capture equates to killing if the civilian is uncapturable, otherwise double the value
            fun captureValue() = abs(killValue()) * if (civilianUnit.hasUnique(UniqueType.Uncapturable)) 1 else 2
            
            
            
            // if melee is attacking, or ranged unit can move to the tile
            if (militaryUnit != null || attacker.movement.canReachInCurrentTurn(attackTile.tileToAttack))
                // capture, or kill if uncapturable
                attackValue += captureValue()
            // ranged attack against civilian
            else {
                val damageToDefender = BattleDamage.calculateDamageToDefender(MapUnitCombatant(attacker),MapUnitCombatant(civilianUnit))
                    .coerceAtMost(civilianUnit.health)
                attackValue +=
                    if (damageToDefender < civilianUnit.health) damageValue(damageToDefender) 
                    else killValue()
            }
        }

        // todo different logic for ranged units?
        // Prioritise closer units as they are generally more threatening to this unit
        // Moving around less means we are straying less into enemy territory, and allows cavalry to retreat
        val movementCost = attacker.currentMovement - attackTile.movementLeftAfterMovingToAttackTile
        attackValue -= (5 * movementCost).roundToInt()
        // compensate a little
        attackValue += 5
        
        return attackValue
    }

    /**
     * Returns a value which represents the attacker's motivation to attack a unit.
     * Base value is 100 and standard deviation is around 80 to 130
     */
    @Readonly
    private fun getUnitAttackValue(attacker: MapUnit, attackTile: AttackableTile): Int {
        if (attacker.civ.civName == Constants.simulationCiv2)
            return getUnitAttackValueNew(attacker, attackTile)
        // Base attack value, there is nothing there...
        var attackValue = Int.MIN_VALUE
        // Prioritize attacking military
        val militaryUnit = attackTile.tileToAttack.militaryUnit
        val civilianUnit = attackTile.tileToAttack.civilianUnit
        if (militaryUnit != null) {
            attackValue = 100
            // Associate enemy units with number of hits from this unit to kill them
            val attacksToKill = (militaryUnit.health.toFloat() /
                BattleDamage.calculateDamageToDefender(MapUnitCombatant(attacker), MapUnitCombatant(militaryUnit)))
                .coerceAtLeast(1f).coerceAtMost(10f)
            // We can kill them in this turn
            if (attacksToKill <= 1) attackValue += 30
            // On average, this should take around 3 turns, so -15
            else attackValue -= (attacksToKill * 5).toInt()
        } else if (civilianUnit != null) {
            attackValue = 50
            // Only melee units should really attack/capture civilian units, ranged units may be able to capture by moving
            if (attacker.baseUnit.isMelee() || attacker.movement.canReachInCurrentTurn(attackTile.tileToAttack)) {
                if (civilianUnit.isGreatPerson()) {
                    attackValue += 150
                }
                if (civilianUnit.hasUnique(UniqueType.FoundCity, GameContext.IgnoreConditionals)) attackValue += 60
            } else if (attacker.baseUnit.isRanged() && !civilianUnit.hasUnique(UniqueType.Uncapturable)) {
                return 10 // Don't shoot civilians that we can capture!
            }
        }
        // Prioritise closer units as they are generally more threatening to this unit
        // Moving around less means we are straying less into enemy territory
        // Average should be around 2.5-5 early game and up to 35 for tanks in late game
        attackValue += (attackTile.movementLeftAfterMovingToAttackTile * 5).toInt()

        return attackValue
    }
}

import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(3, 5) == 8)
            assert(Exercises.sumOfDivBy3Or5(1, 2) == 0)
            assert(Exercises.sumOfDivBy3Or5(6, 10) == 25)
            assert(Exercises.sumOfDivBy3Or5(15, 15) == 15)
            assert(Exercises.sumOfDivBy3Or5(-5, 5) == 0)
        }
        'test_primeFactor - {
            assert(Exercises.primeFactor(2 * 2 * 2 * 5 * 5) == Seq(2, 5))
            assert(Exercises.primeFactor(2 * 7) == Seq(2, 7))
            assert(Exercises.primeFactor(17) == Seq(17))
            assert(Exercises.primeFactor(17 * 17) == Seq(17))
            assert(Exercises.primeFactor(1) == Seq())
        }
        'test_sumByFunc - {
            assert(Exercises.sumScalars(Exercises.Vector2D(1, 2), Exercises.Vector2D(4, 1), Exercises.Vector2D(2, 2), Exercises.Vector2D(4, 3)) == 20)
            // cos = 1, then cos = -1 
            val res = Exercises.sumCosines(Exercises.Vector2D(0, 1), Exercises.Vector2D(0, 1), Exercises.Vector2D(1, 1), Exercises.Vector2D(-1, -1))
            assert((res).abs < 1e-6)
        }
        'test_sortByHeavyweight - {
            assert(Exercises.sortByHeavyweight(Exercises.balls) == Seq("Tin", "Platinum", "Nickel", "Aluminum", "Titanium", "Lead", "Sodium",
                "Uranium", "Gold", "Tungsten", "Zirconium", "Chrome", "Iron", "Copper", "Silver", "Plutonium", "Cobalt", "Cesium", "Calcium",
                "Lithium", "Magnesium", "Potassium", "Graphite"))
        }
    }
}

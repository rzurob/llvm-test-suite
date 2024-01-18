!*   create a module 
MODULE Mod
      INTEGER, PARAMETER :: K = 10, P = 100

      CONTAINS

      SUBROUTINE Sub1(Arg)
        INTEGER :: Arg, I

        open(unit=10, file='file01')
        write(10, *) 1,' line written'

        DO I = 2, Arg
          write(10, *) I,' lines written'
        END DO
      END SUBROUTINE Sub1
END MODULE

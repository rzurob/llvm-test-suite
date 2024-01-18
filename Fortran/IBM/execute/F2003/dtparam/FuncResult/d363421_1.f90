MODULE Mod 
      TYPE Base (l1)
        INTEGER, LEN  :: l1

        CHARACTER(l1) :: C0 
        INTEGER :: A0(l1)
      END TYPE

      CONTAINS

      FUNCTION func(b2) 
        CLASS(Base(*)) :: b2
        CLASS(Base(b2%l1)), ALLOCATABLE :: func 

!        print*, b2%A0 
!        print*, b2%C0  
        ALLOCATE (func, source = b2) 

      END FUNCTION   
END MODULE
      USE Mod

      call foo (func( Base(3) (C0='XLF',A0= 3) ) )
      contains

      subroutine foo (s)
        class(base(*)), intent(in) :: s
      SELECT TYPE ( s )

         CLASS IS (Base(*))
            print *, s%a0
            print *, s%c0

         CLASS DEFAULT
            STOP 11
      END SELECT
      end subroutine
END

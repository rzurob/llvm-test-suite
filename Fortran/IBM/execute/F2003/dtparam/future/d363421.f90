MODULE Mod 
      TYPE Base (l1)
        INTEGER, LEN  :: l1

        INTEGER :: A0(l1)
        CHARACTER(l1) :: C0 
      END TYPE

      CONTAINS

      FUNCTION func(b2) 
        CLASS(Base(*)) :: b2
        CLASS(Base(b2%l1)), ALLOCATABLE :: func 

        print*, b2%C0  
        print*, b2%A0 
        ALLOCATE (func, source = b2) 

      END FUNCTION   
END MODULE
      USE Mod

      SELECT TYPE ( s => func( Base(3) (C0='XLF',A0= 3) ) )
         CLASSIS (Base(*))

         CLASSDEFAULT
            STOP 11
      END SELECT
END

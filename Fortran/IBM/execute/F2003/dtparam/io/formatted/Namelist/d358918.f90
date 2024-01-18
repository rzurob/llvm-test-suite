      TYPE Base (l1)
        INTEGER, LEN  :: l1
        INTEGER :: I
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child
        REAL :: Rarr  
      END TYPE Child

      CLASS(Base(:)), ALLOCATABLE        :: b1

      ALLOCATE(child(2) :: b1)

      call sub(b1)

      CONTAINS
      SUBROUTINE sub(argb1)
       TYPE(Base(*))            :: argb1

       call sub2 (argb1)
      END SUBROUTINE sub

      SUBROUTINE sub2 (x)
       class(base(*)) x

       select type (x)
         type is (base(*))
           
         type is (child(*))
           STOP  1
       end select

      end SUBROUTINE
END

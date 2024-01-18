module m
      TYPE Base (l1)
        INTEGER, LEN  :: l1
        INTEGER :: I

        contains

        procedure :: print => printBase
        procedure, non_overridable :: printWithHeader
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child
        REAL :: Rarr

        contains

        procedure :: print => printChild
      END TYPE Child

    contains

    subroutine printWithHeader (b)
        class(base(*)), intent(in) :: b

        print *, '------'
        call b%print
    end subroutine

    subroutine printBase (b)
        class(base(*)), intent(in) :: b

        print *, b%i
    end subroutine

    subroutine printChild (b)
        class(child(*)), intent(in) :: b

        print *, b%i, b%rarr
    end subroutine
end module

use m
      CLASS(Base(:)), ALLOCATABLE        :: b1

      ALLOCATE(b1, source=child(2)(10, -1.0))

      call sub(b1)

      CONTAINS
      SUBROUTINE sub(argb1)
       TYPE(Base(*))            :: argb1

        call argb1%print
        call argb1%printWithHeader

      END SUBROUTINE sub
END


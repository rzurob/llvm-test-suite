! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*                               TO is intent(in) argument.
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


    integer, allocatable :: TO, FROM

    allocate ( from, source = 10 )

    call sub(from, to)

    print *, to
    contains
        subroutine sub(a, b)
          integer, allocatable :: a, b
          intent(inout) a
          intent(in)  b
          call move_alloc (a, b)
        end subroutine
    end

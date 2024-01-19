! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : 1.FROM/TO are of type real
!*                               2.TO is function name which is interface name
!*                                  of a procedure declaration
!*                               3.FROM associated with real pointer
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

        real, pointer :: p

        contains
            real function func1(arg)
                real, allocatable :: arg
                allocatable :: func1
                target :: func1

                call move_alloc(arg,func1)

                if ( .not. associated(p, func1)) error stop 23

            end function

end module

    program main
        use m
        procedure(func1), pointer ::  iP
        real, target, allocatable :: av
        logical precision_R4

        iP => func1

        allocate(av, source= real(31))

        p => av

        if (  .not. precision_R4(iP(av), real(31)) ) error stop 33
        if ( allocated(av) ) error stop 35
end

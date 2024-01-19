! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 1/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : pointer assignment (unlimited poly-pointer assigned to an
!*                               object returned from a function call; intrinsic
!*                               types)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fpAssgn004a
    interface
        function iT (i)
            integer*4, pointer :: iT
            integer*4, intent(in) :: i
        end function

        function fT (r)
            real*4, pointer :: fT
            real*4, intent(in) :: r
        end function
    end interface

    class(*), pointer :: x

    x => iT (0)     ! <-- null()
    if (associated (x)) error stop 1_4

    x => iT (-10)   ! <-- null()
    if (associated (x)) error stop 2_4

    x => iT (100)
    if (.not. associated (x)) error stop 3_4

    x => fT (1.1)   ! <-- null()
    if (associated (x)) error stop 4_4

    x => fT (-1.0)
    if (.not. associated (x)) error stop 5_4
end

function iT (i)
    integer*4, pointer :: iT
    integer*4, intent(in) :: i

    if (i <= 0) then
        iT => null()
    else
        allocate(iT)
        iT = i
    end if
end function

function fT (r)
    real*4, pointer :: fT
    real*4, intent(in) :: r

    if (r > 0.0) then
        fT => null()
    else
        allocate (fT)
        fT = r
    end if
end function

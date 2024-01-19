!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-tar is a procedure pointer component
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base
        character(:), pointer :: ptr(:,:)
        procedure(func), pointer, nopass :: pp
    end type

    contains
        function func(ch)
            character(3) :: ch(8)
            character(3), pointer :: func(:,:)

            if ( .not. associated(func)) then
                allocate(func(2,4), source = reshape(ch(8:1:-1), (/2,4/)))
            endif

        end function
end module

program main

    use m

    type(base), target :: b
    character(3) :: ch(8)

    ch = (/'IBM','---','XLF','---','XLC','---','AIX','---' /)

    ! procedure pointer
    b%pp => func

    ! data pointer assignmnt
    b%ptr(len(ch(1)):, size(ch):) => b%pp(ch)
    b%ptr(len(ch(1)):, size(ch):) => b%ptr(len(ch(1))+1::2,:)

    if ( .not. associated(b%ptr)) error stop 2
    if ( any( lbound(b%ptr) .ne. (/3, 8/) )) error stop 5
    if ( any( ubound(b%ptr) .ne. (/3, 11/) )) error stop 8
    print *, b%ptr
    print *, b%ptr <= reshape ( &
        (/'ibm','---','xlf','---','xlc','---','aix','---' /), (/2,4/))

end program


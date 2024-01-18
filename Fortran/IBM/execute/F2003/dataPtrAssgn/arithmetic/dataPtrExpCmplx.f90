!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrExpCmplx.f 
!*
!*  PROGRAMMER                 : Michelle Zhang
!*  DATE                       : Aug 31, 2006
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION
!*
!* - defined-assignment for type complex with diff ranks on RHS/LHS of = 
!* - data-ptr assgn w bounds-remapping-lst appears in the sub for defined assgn 
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program main

    interface assignment(=)
    subroutine mydefine(a,b)
        complex(4), intent(in) :: b(100) 
        complex(4), pointer, intent(out) :: a(:,:) 
    end subroutine
    end interface

    complex(4) ::  b(100)
    complex(4), pointer :: a(:,:)

    b = (/( cmplx(i-1,i+1,4), i=1,100  )/)

    a = b

    if ( .not. associated(a)) stop 1 
    if ( any (lbound(a) .ne. (/2,3/))) stop 2
    if ( any (ubound(a) .ne. (/11,12/))) stop 3 
    write (*, '("(",f10.6,", ", f10.6, ")")') a 
    write (*, '("(",e12.6,", ", e12.6, ")")') a**2 

end program

subroutine mydefine(a,b)

    complex(4), intent(in) :: b(100) 
    complex(4), pointer, intent(out) :: a(:,:) 

    complex(4), pointer :: c(:)

    allocate (c(100), source=b)

    a(2:11,3:12) => c(100:1:-1)
end subroutine

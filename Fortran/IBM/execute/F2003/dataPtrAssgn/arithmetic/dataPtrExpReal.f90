!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrExpReal.f 
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
!* - lb/ub of data-ptr is defined by enum construct
!* - data-ptr/data-target declared in diff level of modules
!* - pointer => is used in external proc
!* 
!234567890123456789012345678901234567890123456789012345678901234567890

module mod
    real, pointer :: ptr(:)
end module

module m
    use mod
    type base
        real, allocatable :: tar(:)
    end type
end module

module n
    use m

    enum, bind(c)
        enumerator :: lb=7, stride=8, size
    end enum

    type(base), target, allocatable :: b1

    interface foo
        subroutine sub()
            import b1, ptr
        end subroutine

    end interface
end module

program main
    use n

    allocate(b1)
    allocate(b1%tar(100), source=(/ ( real(i,4), i=1, 100)/))

    call foo
    if ( .not. associated (ptr, b1%tar(21:100:8)) ) stop 20
    if ( lbound(ptr,1) /= 7 ) stop 21
    if ( ubound(ptr,1) /= 16 ) stop 23
    write(*, '(5f15.8)') ptr**2

end program

subroutine sub
    use mod, only: ptr
    use n, only:lb,stride,size,b1

    ptr(lb:lb+size) => b1%tar (21::stride)
end subroutine

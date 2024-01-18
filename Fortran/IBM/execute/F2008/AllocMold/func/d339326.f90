!* ===================================================================
!* XL Fortran Test Case                          IBM INTERNAL USE ONLY
!* ===================================================================
!* 
!* TEST CASE TITLE            : d339326
!* 
!* ORIGINAL PROGRAMMER        : Jim Xia
!* PROGRAMMER                 : Izhak Jakov
!* 
!* DATE                       : June 2, 2015
!* ORIGIN                     : AIX Compiler Development,
!*                            : IBM Software Solutions Toronto Lab
!* 
!* DRIVER STANZA              : xlf2008
!* REQUIRED COMPILER OPTIONS  :
!* 
!* KEYWORD(S)                 :
!* TARGET(S)                  :
!* NUMBER OF TESTS CONDITIONS :
!* 
!* DESCRIPTION                :
!* 
!* TEST CASE ADAPTED FROM     : $(tsrcdir)/F2003/dtparam/allocate/d339326.f
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type A ( n )
        integer, len :: n
        integer  val(n)
    end type
end module

subroutine test (n, a1, a2)
use m
    type(A( n )), allocatable :: a1
    type(A( n )), allocatable :: a2

    allocate (a1, a2, source=A(n)([(i, i=1, n)])) 
    
end subroutine

program main
use m
    integer,        parameter   :: nsize = 10
    type(A(nsize)), allocatable :: a1, a2

    abstract interface
        subroutine procInterface (n, a1, a2)
        import
            type(A( n )), allocatable :: a1
            type(A( n )), allocatable :: a2
        end subroutine
    end interface

    procedure(procInterface) test

    call test (nsize, a1, a2)

    if (.not. allocated(a1)) error stop 1_4
    if (.not. allocated(a2)) error stop 2_4

print *, a1
    if (any(a1%val /= [(i, i=1,nsize)])) error stop 3_4
    if (any(a2%val /= [(i, i=1,nsize)])) error stop 4_4
end

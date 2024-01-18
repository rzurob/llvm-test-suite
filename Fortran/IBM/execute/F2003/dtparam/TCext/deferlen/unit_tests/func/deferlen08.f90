! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qdeferredlp /tstdev/F2003/deferlen/unit_tests/func/deferlen08.f
! opt variations: -qnock -qnok -qnol -qnodeferredlp

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Deferred Character Length
!*
!*  PROGRAMMER                 : James Ren
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  DRIVER STANZA              : xlf90/95
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the allocatable attributes on
!*                               characters with deferred length.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
type :: student(k1,n1)    ! (4,20)
    integer, kind :: k1
    integer, len  :: n1
   character(:), allocatable :: name
end type student

type( student(4,:)), pointer :: pStu
type( student(4,20)), target :: tStu

allocate (character(20)::tStu%name)
tStu%name = "Don"

pStu=>tStu
if  (pStu%name /= 'Don') error stop 1

deallocate (tStu%name)

end

! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign002d.f
! opt variations: -qnol -qnodeferredlp

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign013.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2006s
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=polymorphic
!*
!*  DESCRIPTION                :C717 - diagnostic
!*
!234567890133456789013345678901334567890133456789013345678901334567890

  type dt1(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    real(k1)      :: num1
  end type

  class(dt1(:,4)), pointer :: ptr1(:)
  class(*), target, allocatable :: arr1(:)
  class(*), pointer :: ptr2(:)
  type(dt1(:,4)), pointer :: ptr3(:)

  allocate(arr1(5), source=(/1,1,1,1,1/))

  ptr1(5:)=>arr1
  ptr1(10:14)=>ptr2
  ptr3(15:19)=>arr1
  ptr3(15:)=>ptr2

end

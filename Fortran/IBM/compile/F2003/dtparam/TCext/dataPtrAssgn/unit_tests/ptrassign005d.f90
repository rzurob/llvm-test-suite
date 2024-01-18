! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign005d.f
! opt variations: -qnol -qnodeferredlp

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign005.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :C719
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  type dt(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)   :: num1
  end type

  type(dt(:,4)) , pointer :: ptr1(:,:,:)

  type(dt(20,4)), target :: arr2(1:10)

  !incorrect rank for pointer
  ptr1(4:8,5:9,6:10,7:11)=>arr1

  ptr1(4:8,5:9)=>arr1

  ptr1(4:8,5:9,6:10,7:11)=>arr1

  ptr1(4:8,5:9)=>arr1


end


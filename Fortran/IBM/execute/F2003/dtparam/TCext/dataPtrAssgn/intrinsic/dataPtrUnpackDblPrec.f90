! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrUnpackDblPrec.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/31/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : data pointer enhancement
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*
!* - data-pointer of type dble precision as arg field of unpack
!* - data-pointer is component of dummy arg
!* - in diff scope, two DTs with same definition
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
      type t(k1,n1)    ! (4,20)
          integer, kind :: k1
          integer, len  :: n1
          sequence
          double precision, pointer :: p(:,:,:,:)
      end type
end module

program main
      type t(k1,n1)    ! (4,20)
          integer, kind :: k1
          integer, len  :: n1
          sequence
          double precision, pointer :: p(:,:,:,:)
          end type
          type(t(4,20)) :: a1

      double precision, allocatable :: a(:,:,:,:)

      interface foo
          subroutine sub(a)
          import t
             type(t(4,*)) a
          end subroutine
      end interface

      call sub(a1)

     if ( .not. associated(a1%p)) error stop 8
     if ( any(lbound(a1%p) .ne. (/0,1,2,3 /) )) error stop 11
     if ( any(ubound(a1%p) .ne. (/0,2,2,3 /) )) error stop 12

     write (*, '(2f15.10)') a1%p

     write (*, '(2f15.10)')  unpack((/6.5_8, 0.1_8/), &
        Reshape((/ .true., .false. /), (/1,2,1,1/)), a1%p)
  end program

subroutine sub(arg)
use m, only : t

    	  type(t(4,*)) :: arg

    	  allocate(arg%p(1,2,2,1))
    	  arg%p = reshape((/1.2_8,2.3_8,3.4_8, 4.5_8 /), (/1,2,2,1/))

    	  arg%p(0:0,1:2, 2:2, 3:3) => arg%p(1,2,1:2,1)

end subroutine

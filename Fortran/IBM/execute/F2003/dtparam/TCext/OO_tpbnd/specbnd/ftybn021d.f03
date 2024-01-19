! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/ftybn021d.f
! opt variations: -qnol

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : overriding
!*
!*  DESCRIPTION                : public the private binding procedure of
!*                               parent in extended type. Binding procedure
!*                               without dummy arguments.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


	module mod
   private
		type, private :: base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: x
		contains
      procedure, nopass :: bind => proc1
		end type base

   	type, extends(base), public :: parent    ! (20,4)
 		contains
      procedure, nopass :: bind => proc2
 		procedure, nopass :: bind_parent => proc2
		end type

      type, extends(parent), public :: child    ! (20,4)
      contains
      procedure, nopass :: bind => proc3
      procedure, nopass :: bind_child => proc3
      end type

      contains
      integer function proc1()
         proc1 = 100
      end function proc1

      integer function proc2()
         proc2 = 200
      end function proc2

      integer function proc3()
         proc3 = 300
      end function proc3
	end module

   use mod

   type(parent(20,4)) :: parent_dt
   type(child(20,4)) :: child_dt

   if(parent_dt%bind() .ne. 200)          error stop 2
   if(parent_dt%bind_parent() .ne. 200)   error stop 3
   if(child_dt%bind() .ne. 300)           error stop 4
   if(child_dt%bind_child() .ne. 300)     error stop 5


  end


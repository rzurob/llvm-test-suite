! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp /tstdev/F2003/mv_Alloc/typCompatible/realkind8a.f
! opt variations: -qnol -qdeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of type real*8
!*                               TO is  of unlimited poly
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
      type base(n1,k1)    ! (20,8)
          integer, kind         :: k1
          integer, len          :: n1
          class(*), allocatable :: r
          real(k1), allocatable :: var
      end type

      interface
           subroutine sub(arg)
               import base
               type(base(*,8)) :: arg
           end subroutine
      end interface
end module

      use m

      logical precision_r8

      type(base(20,8)), allocatable :: b

      allocate(b)

      call sub(b)

      call move_alloc(b%var, b%r)

      if ( allocated(b%var) ) stop 21
      if ( .not. allocated(b%r) ) stop 25

      select type( x => b%r )
            type is (real*8)
                write (*, '(f15.8)' ) b%var
      end select

      end

           subroutine sub(arg)
               use m, only:base
               type(base(*,8)) :: arg
               real*8, allocatable :: r1

               allocate(arg%r, source = dprod(1.1_4, 2.2_4) )
               allocate(r1, source=-3.3_8)
               allocate(arg%var, source = r1 )

           end subroutine

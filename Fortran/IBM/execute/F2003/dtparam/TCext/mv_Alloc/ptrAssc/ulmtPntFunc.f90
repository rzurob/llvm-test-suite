! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp /tstdev/F2003/mv_Alloc/ptrAssc/ulmtPntFunc.f
! opt variations: -qnock -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is non-poly, dummy arg, type child
!*                               TO is poly, dummy arg, type base
!*                               pointer is class(*), module func name
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

      type  :: base(n1,k1)    ! (20,4)
          integer, kind :: k1
          integer, len  :: n1
          integer(k1)      id
      end type

      type, extends(base) :: child    ! (20,4)
          character(:), allocatable :: ch
      end type

      contains

         class(*) function func(arg,brg)
            type(child(:,4)) :: arg
            class(base(:,4)) :: brg
            allocatable arg, brg
            pointer func
            target arg, brg

            func => arg

            call move_alloc(arg, brg)

            if ( .not. allocated(brg) ) stop 11
            if ( allocated(arg) ) stop 13

            if (.not. associated (func, brg)) stop 23

         end function

end module

      use m

      class(base(:,4)), allocatable :: b
      type(child(:,4)), allocatable :: d

      allocate(b, source= (base(20,4)(6))  )
      allocate(d, source= (child(20,4)(8, 'XYZ')) )

      select type ( x => func(d, b) )
          type is ( child(*,4) )
              if ( x%id /= 8 ) stop 21
              if ( x%ch /= 'XYZ' ) stop 23
          class default
              stop 25
      end select

      end

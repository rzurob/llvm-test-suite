! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/mv_Alloc/typCompatible/dtype4both.f
! opt variations: -qck -qnok -ql -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of a DT, an optional dummy arg
!*                               of a type bound proc
!*                               TO is of the same DT as FROM
!*                               TO is function return name
!*                               move_alloc called in a type bound proc
!*                               defect 321971
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

      type A(k1)    ! (4)
          integer, kind :: k1
          character(:), allocatable :: ch
      end type

      type  :: base(k2)    ! (4)
          integer, kind            :: k2
          type(A(k2)), allocatable :: a1
          contains
              procedure :: get_alloc  => func
      end type

      contains
         type(base(4)) function func(arg, brg)
            class(base(4)) :: arg
            type(base(4)), target :: brg
            optional :: brg
            allocatable func, brg

            if ( present(brg))  then
               if ( .not. allocated(brg))  allocate(brg, source = arg)
               call move_alloc(brg,func)
            end if

         end function

end module

      use m

      type(base(4)), allocatable :: b

      type(base(4)) d

      allocate(b, source=( base(4) ( A(4)('XYZ') ) ) )

      d =  b%get_alloc( b )

      if ( allocated(b)) stop 11

      if ( d%a1%ch /= 'XYZ' ) stop 21

      end

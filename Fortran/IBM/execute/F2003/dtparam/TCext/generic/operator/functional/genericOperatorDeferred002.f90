! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorDeferred002.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Operator: Deferred specific binding
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type, abstract :: pt(k1,n1)    ! (4,20)
       integer, kind :: k1
       integer, len  :: n1
      contains
         procedure(i), deferred :: subtr
         generic :: operator(-) => subtr
   end type

   type, extends(pt) :: onedpt    ! (4,20)
      integer(k1) :: x
      contains
         procedure :: subtr => subtr1d1d
   end type

   type, extends(pt) :: twodpt    ! (4,20)
      integer(k1) :: x, y
      contains
         procedure :: subtr => subtr2d2d
   end type

   interface
      class(pt(4,:)) function i ( a, b )
         import pt
         class(pt(4,*)), intent(in) :: a
         class(pt(4,*)), intent(in) :: b
         pointer :: i
      end function
   end interface

   contains

   class(pt(4,:)) function subtr1d1d ( a, b )
      class(onedpt(4,*)), intent(in) :: a
      class(pt(4,*)), intent(in) :: b
      pointer :: subtr1d1d

      select type ( b )
         type is ( onedpt(4,*) )
            allocate ( subtr1d1d, source = onedpt(4,20) ( x = ( a%x - b%x ) ) )
         type is ( twodpt(4,*) )
            allocate ( subtr1d1d, source = onedpt(4,20) ( x = ( a%x - b%x ) ) )
      end select

   end function

   class(pt(4,:)) function subtr2d2d ( a, b )
      class(twodpt(4,*)), intent(in) :: a
      class(pt(4,*)), intent(in) :: b
      pointer :: subtr2d2d

      select type ( b )
         type is ( twodpt(4,*) )
            allocate ( subtr2d2d, source = twodpt(4,20) ( x = ( a%x - b%x ), y = ( a%y - b%y ) ) )
         type is ( onedpt(4,*) )
            allocate ( subtr2d2d, source = twodpt(4,20) ( x = ( a%x - b%x ), y = a%y ) )
      end select

   end function

end module

program genericOperatorDeferred002
   use m

   class(pt(4,:)), allocatable     :: p0, p01, p11, p21
   class(onedpt(4,:)), pointer     :: p1
   class(twodpt(4,:)), allocatable :: p2

   allocate ( p0, source = onedpt(4,20)(10) )
   allocate ( p1, source = onedpt(4,20)(20) )
   allocate ( p2, source = twodpt(4,20)(30,40) )

   allocate ( p01, source = ( p0 - p1 ) )

   select type ( p01 )
      type is ( onedpt(4,*) )
         if ( p01%x /= -10 ) error stop 1_4
      class default
         error stop 2_4
   end select

   deallocate ( p01 )
   allocate ( p01, source = ( p0 - p2 ) )

   select type ( p01 )
      type is ( onedpt(4,*) )
         if ( p01%x /= -20 ) error stop 3_4
      class default
         error stop 4_4
   end select

   deallocate ( p01 )
   allocate ( p01, source = ( p1 - p2 ) )

   select type ( p01 )
      type is ( onedpt(4,*) )
         if ( p01%x /= -10 ) error stop 5_4
      class default
         error stop 6_4
   end select

   deallocate ( p01 )
   allocate ( p01, source = ( p2 - p1 ) )

   select type ( p01 )
      type is ( twodpt(4,*) )
         if ( ( p01%x /= 10 ) .or. ( p01%y /= 40 ) )  error stop 7_4
      class default
         error stop 8_4
   end select

   allocate ( p11, source = ( p1 - onedpt(4,20) ( 1000 ) ) )
   select type ( p11 )
      type is ( onedpt(4,*) )
         if ( p11%x /= -980 ) error stop 9_4
      class default
         error stop 10_4
   end select

   allocate ( p21, source = ( p2 - onedpt(4,20) ( 100 ) ) )
   select type ( p21 )
      type is ( twodpt(4,*) )
         if ( ( p21%x /= -70 ) .or. ( p21%y /= 40 ) ) error stop 11_4
      class default
         error stop 12_4
   end select

   deallocate ( p21 )

   allocate ( p21 , source = ( p2 - twodpt(4,20)(-30,-40)) )
   select type ( p21 )
      type is ( twodpt(4,*) )
         if ( ( p21%x /= 60 ) .or. ( p21%y /= 80 ) ) error stop 13_4
      class default
         error stop 14_4
   end select

end program

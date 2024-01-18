!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 06/07/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        Try procedure pointer pointing C procedure when scalar/array dummy argument and
!*                                        function return bind(c) types (or struct types defined in C with typedef) containing
!*                                        components defined with typedef
!*
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

   use ISO_C_BINDING

   type, bind(C) :: C_BASE
      integer(C_INT)  :: i
      logical(C_BOOL) :: b
   end type

   type, bind(C) :: C_BASE2
      real(C_FLOAT) :: r(3)
   end type

   interface
      integer(C_INT) function getint(dtv) bind(C)
         import C_BASE, C_INT
         type(C_BASE), intent(in) :: dtv
      end function
   end interface

   interface
      real(C_FLOAT) function getfirstR(dtv) bind(C, name='real')
         import C_BASE2, C_FLOAT
         type(C_BASE2), intent(in) :: dtv(3)
      end function
   end interface

   procedure(getint), pointer :: pp1
   procedure(getfirstR), pointer :: pp2

end module

program bindc019
   use m

   type(C_BASE) :: b1
   type(C_BASE2), pointer :: b2(:)

   integer(C_INT) :: i1
   real(C_FLOAT)  :: r1
   logical :: precision_r4

   b1%i = 1001_C_INT
   b1%b = .true._C_BOOL

   allocate ( b2(3) )

   b2(1)%r = (/ 101.0_C_FLOAT, 102.0_C_FLOAT, 103.0_C_FLOAT /)
   b2(2)%r = (/ 111.0_C_FLOAT, 112.0_C_FLOAT, 113.0_C_FLOAT /)
   b2(3)%r = (/ 121.0_C_FLOAT, 122.0_C_FLOAT, 123.0_C_FLOAT /)

   pp1 => getint
   pp2 => getfirstR

   i1 = pp1( b1 )
   r1 = pp2( b2 )

   if ( i1 /= 1001 ) error stop 1_4
   if ( .not. precision_r4( r1, 101.0 ) ) error stop 2_4

end

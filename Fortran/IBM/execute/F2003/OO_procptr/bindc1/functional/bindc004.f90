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
!*                                        Procedure Pointer pointing at C functions with BIND(C) function interface, pass by reference
!*                                        and try BIND(C, NAME='xxx'), and try reversing the order of specifying dummy arg by supplying specifiers
!*                                        and try supplying parameter as dummy argument
!*                                        - dummy argument and function result with REAL (C_FLOAT, C_DOUBLE, C_LONG_DOUBLE)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module cinterfaces

   use ISO_C_BINDING

   interface
      real(C_FLOAT) function getcirclearea(r,pi) BIND(C,NAME='carea')
         import C_FLOAT, C_DOUBLE, C_LONG_DOUBLE
         real(C_DOUBLE), intent(in), VALUE         :: r
         real(C_LONG_DOUBLE), intent(in), VALUE    :: pi
     end function
   end interface

   interface
      real(C_FLOAT) function getrectarea(l,w) BIND(C,NAME='rarea')
         import C_FLOAT, C_DOUBLE, C_LONG_DOUBLE
         real(C_DOUBLE), intent(in), VALUE         :: l
         real(C_LONG_DOUBLE), intent(in), VALUE    :: w
     end function
   end interface

end module

   use cinterfaces

   real(C_LONG_DOUBLE), parameter :: myPI = 3.1416

   interface
      real(C_FLOAT) function itf(r1, r2) BIND(C)
         import C_FLOAT, C_DOUBLE, C_LONG_DOUBLE
         real(C_DOUBLE), intent(in), VALUE         :: r1
         real(C_LONG_DOUBLE), intent(in), VALUE    :: r2
     end function
   end interface

   procedure(itf), pointer, BIND(C) :: p1, p2
   real(C_FLOAT) :: area = 0

   p1 => getcirclearea
   p2 => getrectarea

   area = p1( r2= myPI, r1 = 5.0_C_DOUBLE )
   write ( *, "(1X,L1,1X,F5.2)") associated(p1), area

   area = p2( r2 = 2.0_C_LONG_DOUBLE, r1 = 6.0_C_DOUBLE )
   write ( *, "(1X,L1,1X,F5.2)") associated(p2), area

   nullify(p2)
   p1 => p2

   print *, associated(p1), associated(p2), associated(p1, p2)

end

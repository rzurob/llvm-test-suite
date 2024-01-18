! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self /tstdev/F2003/generic/operator/functional/genericOperatorImplicit002.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Binary Operator: Implicit statement with polymorphic types
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

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: x = 2
      integer(k1)   :: y = 4
      contains
         procedure, pass :: mybmul
         procedure, pass :: mybdiv
         generic :: operator(*) => mybmul
         generic :: operator(/) => mybdiv
   end type

   type, extends(base) :: child    ! (4)
   end type

   contains

   function mybmul ( a, b )
      class(base(4)), intent(in) :: a
      class(base(4)), intent(in) :: b

      type(base(4)) :: mybmul

      mybmul%x = a%x * b%x
      mybmul%y = a%y * b%y

      print *, 'mybmul'

   end function

   function mybdiv ( a, b )
      class(base(4)), intent(in) :: a
      class(base(4)), intent(in) :: b

      type(base(4)) :: mybdiv

      mybdiv%x = a%x / b%x
      mybdiv%y = a%y / b%y

      print *, 'mybdiv'

   end function

end module

program genericOperatorImplicit002
   use m

   implicit class(base(4))  (a-i)
   implicit class(child(4)) (j-z)

   allocatable :: a,b,c,x,y,d

   allocate ( b,c,y,x )

   allocate ( a, source = b*c )
   allocate ( d, source = x*y )

   print *, a%x, a%y
   print *, d%x, d%y

   deallocate ( a,d )

   allocate ( a, source = b/c )
   allocate ( d, source = x/y )

   print *, a%x, a%y
   print *, d%x, d%y

end program

!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: The derived-type-spec shall not specify an abstract type (C401)
!*                                        Structure Constructor appears in an associate construct
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

   type, abstract :: base(k1)
      integer, kind :: k1
      integer(k1) :: id
   end type

   type, extends(base) :: child(k2)
      integer, kind :: k2
      real(k2) :: rid
   end type

   type, abstract :: base2(n)
      integer, len :: n
      character(n) :: c
   end type

end module

program structConstr005
   use m, newbase => base

   associate ( aa => newbase(4)(10) )
   end associate

   associate ( gg => base2(3)('ibm') )
   end associate

end program

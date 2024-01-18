!*  ===================================================================
!*
!*  DATE                       : 06/07/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        C1218: BIND(C) specified, but no interface
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
      procedure, BIND(C), pointer :: p1
      procedure, BIND(C) :: p2
   end module

   procedure(interface), BIND(C), pointer :: p3
   procedure(interface), BIND(C) :: p4

end

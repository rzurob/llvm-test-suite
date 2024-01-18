! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (length type parameter has to be the
!                               same between the type-spec and the allocate
!                               object)
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

program falloc001d4
    character, pointer :: ch(:)

    allocate (character(10) :: ch(2))
end

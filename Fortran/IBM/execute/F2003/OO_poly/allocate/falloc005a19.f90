! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (level-3 expression in the
!                               source-expr: concat-op involved)
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

program falloc005a19
    character(10) :: c1 (2, 2)

    character(20), allocatable :: c2(:), c3(:,:)

    c1 = reshape ((/'c1_1','c1_2','c1_3','c1_4'/), (/2,2/))

    allocate (c2(4), source=reshape (c1,(/4/)) // (/'1','2','3','4'/))

    allocate (c3(2,2), source=c1//reshape((/'01','02','03','04'/), (/2,2/)))

    print *, c2
    print *, c3
end

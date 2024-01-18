! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*                               TO has different rank from FROM
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


    character(:), allocatable :: TO(:,:,:,:,:)
    character(8), allocatable :: FROM(:,:,:,:,:,:)

    allocate(FROM(5,5,2,2,2,2))
    call move_alloc (from, to)
    end

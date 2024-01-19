!*  ===================================================================
!*
!*  DATE                       : 30/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Block Data, VOLATILE
!*
!*  DESCRIPTION                : funcitonal TC for  C1117
!*
!*  C1117 (R1116) A block-data specification-part shall contain only
!*        derived-type definitions and ASYNCHRONOUS, BIND, COMMON, DATA,
!*        DIMENSION, EQUIVALENCE, IMPLICIT, INTRINSIC, PARAMETER, POINTER,
!*        SAVE, TARGET, USE, VOLATILE, and type declaration statements.
!* ===================================================================

  program volatileC1117

    integer x
    integer, VOLATILE:: y

    common /cmblk/ x, y

  end program volatileC1117

  block data bk

    integer x
    integer, VOLATILE:: y

    common /cmblk/ x, y

    data x /34/
    data y /67/

  end block data bk

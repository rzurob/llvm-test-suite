!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : contiguous08d.f
!*
!*  PROGRAMMER                 : David Nichols
!*  DATE                       : June 24, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS attribute - C530
!*
!*  DRIVER STANZA              : xlf2008
!*
!*  DESCRIPTION                : Testing proper diagnostics of
!*                               the F2008 attribute
!*                               CONTIGUOUS
!*                               Testing 5.3.7 C530 CONTIGUOUS
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program contiguous08d
        use ISO_C_BINDING

        ! C_LOC, LOC:
        ! If it is an array, it shall be contiguous and have nonzero size

        integer, contiguous, pointer :: ipac(:)
        integer, target              :: ita (9)
        type(C_PTR)                  :: caddr
        integer                      :: laddr

        ipac => ita

        caddr = C_LOC(ita)
        laddr = LOC(ipac)
        laddr = LOC(ita)

        caddr = C_LOC(ipac(1:10:2))
        caddr = C_LOC(ita(1:10:2))
        laddr = LOC(ipac(1:10:2))
        laddr = LOC(ita(1:10:2))
        

      end

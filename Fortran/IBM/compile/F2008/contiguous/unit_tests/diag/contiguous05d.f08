!*  ===================================================================
!*
!*  DATE                       : June 24, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS statement 5.4.6
!*
!*  DESCRIPTION                : Testing proper diagnostics of
!*                               the F2008 statement
!*                               CONTIGUOUS
!*                               Testing 5.4.6 CONTIGUOUS
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program contiguous05d

        ! 5.4.6 CONTIGUOUS statement
        ! R533: contiguous-stmt is CONTIGUOUS [ :: ] object-name-list
        ! The CONTIGUOUS statement specifies the CONTIGUOUS attribute
        ! for a list of objects.

        type base
        end type

        type(base), pointer :: bpac(:)
        integer,    pointer :: ipac(:)
        character,  pointer :: cpac(:)
        contiguous          :: bpac, ipac, cpac

        integer, dimension(5) :: iac
        integer, pointer      :: ipc
        integer               :: ic
        contiguous            :: ic, iac, ipc

        contiguous   :: rc,  rpc, rac, rpac
        real         :: rc,  rpc, rac, rpac
        pointer      :: rpc, rpac
        dimension    :: rac(5)
        dimension    :: rpac(:)

        ! Errors should be generated after all specification statements
        ! Correct:  bpac, ipac, cpac, rpac
        ! Erronous: iac, ipc, ic, rc, rpc, rac
      end

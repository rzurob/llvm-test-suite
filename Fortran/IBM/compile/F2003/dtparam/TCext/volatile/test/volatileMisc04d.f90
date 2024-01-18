! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/volatile/test/volatileMisc04d.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 12/06/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : VOLATILE
!*
!*  DESCRIPTION                : diagnostic TC for  DTIO
!*
!*   9.5.3.7.2:
!*             If the dtio-generic-spec is WRITE(FORMATTED), the characteristics
!*     shall be the same as those specified.....
!*            Use -qxlf2003=novolatile should not have effect to current
!*            behavior.
!* ===================================================================

    module m
       type dt(k1)    ! (4)
           integer, kind            :: k1
           integer(k1), allocatable :: x(:)
           contains

           procedure :: print => printDt
       end type

       interface write(formatted)
          subroutine ForWrite (dtv, unit, iotype, vlist, iostat, iomsg)
          import dt
            class (dt(4)), intent(in), VOLATILE :: dtv
            integer, intent(in), VOLATILE :: unit
            character(*), intent(in), VOLATILE :: iotype
            integer, intent(in), VOLATILE :: vlist(:)
            integer, intent(out), VOLATILE :: iostat
            character(*), intent(inout), VOLATILE:: iomsg
          end subroutine
        end interface
        contains
           subroutine printDt (b, unit)
              class (dt(4)), intent(in) :: b
              integer, intent(in) :: unit

          end subroutine
    end module m


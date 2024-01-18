!*  ===================================================================
!*
!*  DATE                       : June 24, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS attribute 5.3.7.3
!*
!*  DESCRIPTION                : Testing proper diagnostics of
!*                               the F2008 attribute
!*                               CONTIGUOUS
!*                               Testing 5.3.7.3 CONTIGUOUS
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program contiguous04d

        type base
          integer :: baseint
          integer :: contiguitystopper
        end type

        integer,    pointer, contiguous :: ipc(:)
        character,  pointer, contiguous :: cpc(:)

        integer,    target              :: it (5)
        integer,    target              :: itz(0)
        character,  target              :: ct (5)
        type(base), target              :: bt (5)

        integer                         :: zero
        integer                         :: one

        zero = 0
        one  = 1

        ipc => bt%baseint ! Not contiguous

        ! - the elements of the object in array element order are not
        !   consecutive in the elements of the base object,
        ipc => it(5:1:-1)

        cpc => ct(1:0) ! contiguous
        ipc => it(1:0) ! contiguous
        cpc => ct(one:zero) ! contiguous
        ipc => it(one:zero) ! contiguous

        ipc => itz ! contiguous

      end

      module timers_module
        integer, parameter  :: num_timers = 100

        ! these are thread-local in CAF
        integer,   save     :: counters(num_timers)
        integer*8, save     :: ops(num_timers)
        real,      save     :: starts(num_timers)
        real,      save     :: times(num_timers)

      contains

      subroutine timer_init()

        implicit none

        counters(:) = 0
        ops(:)      = 0
        starts(:)   = 0
        times(:)    = 0

      end subroutine timer_init

      subroutine timer_start(n)
      
        implicit none
        integer, intent(in) :: n
        real                :: etime_, elapsed(2)

        starts(n) = etime_(elapsed)

      end subroutine timer_start

      subroutine timer_stop(n)

        implicit none
        integer, intent(in) :: n
        real                :: etime_, elapsed(2)

        times(n)    = times(n) + ( etime_(elapsed) - starts(n) )
        counters(n) = counters(n) + 1

      end subroutine timer_stop

      subroutine timer_stop2(n, lops)

        implicit none
        integer, intent(in) :: n, lops

        call timer_stop(n)                                  ! inlined with O3 -qhot
        ops(n) = ops(n) + lops

      end subroutine timer_stop2

      double precision function timer_total(n)

        implicit none
        integer, intent(in) :: n

        timer_total = times(n)

      end function timer_total

      integer function timer_count(n)

        implicit none
        integer, intent(in) :: n

        timer_count = counters(n)

      end function timer_count

      integer*8 function timer_ops(n)

        implicit none
        integer, intent(in) :: n

        timer_ops = ops(n)

      end function timer_ops

      end module

program stream
    use timers_module

#ifndef TPN
    integer,   parameter :: TPN = 1024*32
#endif

    integer*8, parameter :: megabyte = 2_8 ** 20

#ifndef N
    integer*8, parameter :: N = 119808_8 * megabyte / 8_8 / TPN
#endif

    real(8)              :: rate
    real(8), save        :: a(N)[*], b(N)[*], c(N)[*], alpha

    call timer_init

    call random_number(b)
    call random_number(c)
    call random_number(alpha)

    if (this_image() == 1) then
        print *, "Total memory =", N*num_images()*8/megabyte, "MBs"
        print *, "memory per image =", N*8/megabyte, "MBs"
    end if

    ! warm up
    call stream_triad(a,b,c,alpha)

    sync all
    call timer_start(1)
    sync all

    call stream_triad(a,b,c,alpha)

    sync all
    call timer_stop(1)

    rate = 1.0e-9 * 3 * 8 * N * num_images() / timer_total(1)
    if (this_image() == 1) then
        print *, "Total time =", timer_total(1), "s"
        print *, "Rate =", rate, "GB/s"
    end if

contains

    subroutine stream_triad(a,b,c,alpha)

        real(8), intent(in) :: b(N)[*], c(N)[*]
        real(8), intent(out) :: a(N)[*]
        real(8), intent(in) :: alpha

        a(:) = b(:) + alpha * c(:)

    end subroutine

end program

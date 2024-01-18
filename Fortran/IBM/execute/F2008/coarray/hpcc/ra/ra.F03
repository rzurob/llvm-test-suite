program random_access
    use timers_module

    implicit none
#ifndef NIMAGES
    integer, parameter :: NIMAGES = 8
#endif

#ifndef NNODES
    integer,   parameter :: NNODES            = 1
#endif

#ifndef N
    integer,   parameter :: N                 = 21
#endif

#ifndef K
    integer,   parameter :: K                 = 20
#endif

    integer,   parameter :: images_per_node   = NIMAGES / NNODES

    integer*8, parameter :: table_size        = 2_8 ** N
    integer*8, parameter :: image_size        = table_size / NIMAGES

    integer*8, parameter :: stream_length     = 2_8 ** K
    integer*8, parameter :: updates_per_image = stream_length / NIMAGES

    integer*8, parameter :: table_memory      = table_size * 8
    integer*8, parameter :: image_memory      = image_size * 8
    integer*8, parameter :: node_memory       = image_memory * images_per_node
    integer*8, parameter :: megabyte          = 2_8 ** 20

    integer*8, save      :: T(0:image_size-1)[*]
    integer*8            :: i, a, image, idx
    integer              :: me
    real                 :: gups, latency

    call timer_init

    me = this_image()

    if ( me == 1 ) then
        print *, "Table size =", image_memory/megabyte, "MBytes/CPU,", &
                 table_memory/megabyte, "MB/total on", num_images(), "images"
    end if

    do i=1,image_size
        T(i) = (me-1)*image_size + i
    end do

    sync all

    call timer_start(1)
    call random_access_update

    sync all
    call timer_stop(1)

    gups    = 1.0e-9 * stream_length / timer_total(1)
    latency = timer_total(1) * NIMAGES / stream_length * 1.0e6

    if ( me == 1 ) then
        print *, "Default number of updates (RECOMMENDED) =", stream_length
        print *, "Real time used =", timer_total(1)
        print *, gups, "Billion(10^9) Updates   per second [GUP/s]"
        print *, "Update latency =", latency, "usecs"
    end if

contains

subroutine random_access_update
    integer*8, parameter :: image_shift       = -N
    integer*8, parameter :: image_mask        = NIMAGES - 1
    integer*8, parameter :: index_mask        = image_size - 1
    integer*8 :: i,a,image,idx

    sync all
    a = stream_seed( (me-1)*updates_per_image )
    do i=1,updates_per_image
        a     = stream_next(a)
        image = iand(ishft(a, image_shift), image_mask)
        idx   = iand(a, index_mask)
        T(idx)[image+1] = ieor( T(idx)[image+1], a )
    end do
end subroutine

integer function msb(n)
    integer*8, intent(in) :: n
    do i=63,0,-1
        if (btest(n,i)) msb=i; return
    end do
end function

integer*8 function stream_next(a)
    integer*8, intent(in) :: a
    integer*8, parameter  :: primitive_poly = 7_8

    if ( a<0 ) then
        stream_next = ieor(2*a, primitive_poly)
    else
        stream_next = 2*a
    end if

end function

integer*8 function stream_seed(n)
    integer*8, intent(in) :: n
    integer*8, parameter  :: stream_period = 1317624576693539401_8
    integer*8             :: squares(0:63), k, a

    integer j

    squares(0) = 1_8
    do i=1,63
        squares(i) = stream_next(stream_next(squares(i-1)))
    end do

    k = mod(n,stream_period)
    if (k .eq. 0) then
        stream_seed = 1
    else
        stream_seed = 2
        do i=msb(k)-1,0,-1
            ! in GF(2) the square of the sum is the sum of the squares
            a = 0
            do j=0,63
                if (btest(stream_seed,j)) a = ieor(a, squares(j))
            end do

            if (btest(n,i)) a = stream_next(a)
            stream_seed = a
        end do
    end if
end function

end program

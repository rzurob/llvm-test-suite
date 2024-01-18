! GB DTP extension
! (copy of /tstdev/OO_poly/allocate/falloc018a1_3.f90)

subroutine testAllocation
    use m, only: b1_m, b2_m

    if (allocated (b1_m)) then
        print *, 'b1_m allocated'
        call b1_m%print
    else
        print *, 'b1_m unallocated'
    end if

    if (allocated (b2_m)) then
        print *, 'b2_m allocated with bounds:', lbound(b2_m,1), ':', &
                                                ubound(b2_m,1)

        do i = lbound(b2_m,1), ubound(b2_m,1)
            call b2_m(i)%print
        end do
    else
        print *, 'b2_m unallocated'
    end if
end subroutine

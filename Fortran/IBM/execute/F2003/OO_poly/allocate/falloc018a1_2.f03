
subroutine allocateB1_mB2_m
    use m, only: child, b1_m, b2_m

    if (.not. allocated (b1_m))  allocate (b1_m, source=child(1, 'b1_m'))

    if (.not. allocated (b2_m))  allocate (b2_m(0:1), &
            source=(/(child(i, 'b2_m'), i = 0,1)/))
end subroutine

subroutine deallocateB1_mB2_m
    use m, only: b1_m, b2_m

    if (allocated (b1_m)) deallocate (b1_m)

    if (allocated (b2_m)) deallocate (b2_m)
end subroutine

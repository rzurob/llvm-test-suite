! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-02-25
!*
!*  DESCRIPTION                : added program to verify the output produced
!                               from  sequentialWrite02.f
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program verify
use tEMod1
use tEMod2
    implicit none
    integer, parameter :: N = 5
    integer :: unit, stat,i

    TYPE(tCmpxIntLable1(2,8))  :: cmpxInt2( N )
    TYPE(tCmpxIntLable2(4,12)) :: cmpxInt4( N )
    TYPE(tCmpxIntLable1(8,5))  :: cmpxInt8( N )

    unit = 33
    open (unit, form='unformatted', access='sequential', action='read')

    read (unit, iostat=stat) cmpxInt2

    if (stat /= 0) error stop 10

    read (unit, iostat=stat) cmpxInt4

    if (stat /= 0) error stop 11

    read (unit, iostat=stat) cmpxInt8

    if (stat /= 0) error stop 12

    close (unit)

    do i = 1, N
        if (cmpxInt2(i)%r /= 2*i -1) error stop 13_4
        if (cmpxInt2(i)%i /= 2*i) error stop 14_4
        if (cmpxInt2(i)%lable /= 'CmpxInt2') error stop 15_4

        if (cmpxInt4(i)%r /= 2*i -1) error stop 16_4
        if (cmpxInt4(i)%i /= 2*i) error stop 17_4
        if (cmpxInt4(i)%lable /= 'CMPLX_INT(4)') error stop 18_4

        if (cmpxInt8(i)%r /= 2*i -1) error stop 19_4
        if (cmpxInt8(i)%i /= 2*i) error stop 20_4
        if (cmpxInt8(i)%lable /= 'CI(8)') error stop 21_4
    end do
end program

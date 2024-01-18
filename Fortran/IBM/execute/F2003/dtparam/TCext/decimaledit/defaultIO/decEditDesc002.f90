! GB DTP extension using:
! ftcx_dtp -qnock -qdeferredlp -qreuse=self /tstdev/F2003/decimaledit/defaultIO/decEditDesc002.f
! opt variations: -qnodeferredlp -qreuse=none

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 06/16/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test the DC/CP control edit descriptor on READ
!                               statement; also test .T./.F. effect on logical
!                               under different mode.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1,n2,n3)    ! (4,3,10,20)
        integer, kind  :: k1
        integer, len   :: n1,n2,n3
        integer(k1)       id
        complex(k1)       val(n1)
        logical(k1)       flag(n2)

        character*(n3)    name
    end type

    logical(4), external :: precision_x8
end module

program decEditDesc002
use m
    type (base(4,3,10,20)) :: b1(10)
    class (base(4,:,:,:)), pointer :: b2(:)

    open (1, file='test1', delim='quote')

    allocate (b2(10), source= (/(base(4,3,10,20)(i, (/(cmplx(j, 2*j), j=i,i+2)/), &
            (/(mod(j,2)==0, j=1,10)/), 'xlftest FORTRAN'), i=1,10)/))

    call writeBaseArray (1, b2, 'c')

    rewind 1

    do i = 1, 10
        read (1, '(dc, i10, 3(2g10.3))', advance='no', decimal='PoinT') &
            b1(i)%id, b1(i)%val

        read (1, *, decimal='CommA') b1(i)%flag, b1(i)%name
    end do

    !! now verify the results

    do i = 1, 10
        if (b1(i)%id /= i) error stop 1_4

        do j = i, i+2
            if (.not. precision_x8(b1(i)%val(j-i+1), cmplx(j, 2*j))) &
                    error stop 2_4
        end do

        do j = 1, 10
            if (b1(i)%flag(j) .neqv. mod(j, 2) == 0) error stop 3_4
        end do

        if (b1(i)%name /= 'xlftest FORTRAN') error stop 4_4
    end do

    contains

    subroutine writeBaseArray (unit, b, c)
        integer, intent(in) :: unit
        class(base(4,*,*,*)), intent(in) :: b(:)
        character, intent(in) :: c

        do i = 1, size(b)
            if ((c == 'c') .or. (c == 'C')) then !<-- comma mode
                write(unit, '(dc, I10, 1x, 3(2g10.3))', advance='no') &
                    b(i)%id, b(i)%val
            else        !<-- point mode
                write(unit, '(dp, I10, 1x, 3(2g10.3))', advance='no') &
                    b(i)%id, b(i)%val
            end if

            do j = 1, 10
                if (b(i)%flag(j)) then
                    write (unit, '(dc, ".T.;")', advance='no')
                else
                    write (unit, '(dc, ".F.;")', advance='no')
                end if
            end do

            write (unit, '(dc, 3a)') '"', trim(b(i)%name), '"'
        end do
    end subroutine
end

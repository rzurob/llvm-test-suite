! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/F2003/decimaledit/dtio/dcmlChildWrite009a.f
! opt variations: -qck -qnol -qnodeferredlp -qreuse=self

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
!*  DATE                       : 07/11/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               a variant of dcmlChildWrite009
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1,k2)    ! (20,4,4)
        integer, kind     :: k1,k2
        integer, len      :: n1
        integer(k1)          id
        real(k2), pointer :: data(:) => null()
        character(:), pointer :: name => null()

        contains

        final :: finalizeBase
        procedure :: writeBaseFmtd
        generic :: write(formatted) => writeBaseFmtd
    end type

    contains

    elemental subroutine finalizeBase (b)
        type(base(*,4,4)), intent(inout) :: b

        if (associated(b%data)) deallocate (b%data)

        if (associated(b%name)) deallocate (b%name)
    end subroutine

    subroutine writeBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base(*,4,4)), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(:), allocatable :: fmt

        if (iotype == 'NAMELIST') then
            iostat = 100
            iomsg = 'this routine not targeted for namelist'
            return
        end if

        write (unit, *, iostat=iostat, iomsg=iomsg) dtv%id

        if (iostat /= 0) return

        if (associated(dtv%data)) then
            allocate (character(30) :: fmt)

            do i = lbound(dtv%data, 1), ubound(dtv%data, 1)
                select case (mod(i, 3))
                    case (0)
                        write (fmt, *, iostat=iostat, iomsg=iomsg) '(e12.4)'
                    case (1)
                        write (fmt, *, iostat=iostat, iomsg=iomsg) '(dp,e12.4)'
                    case (2)
                        write (fmt, *, iostat=iostat, iomsg=iomsg) '(dc,e12.4)'

                    case default
                        error stop 10_4
                end select

                if (iostat /= 0) return

                write(unit, fmt, iostat=iostat, iomsg=iomsg) dtv%data(i)
            end do
        else
            write (unit, *, iostat=iostat, iomsg=iomsg) &
                        'data are not associated'
        end if

        if (iostat /= 0) return

        if (associated(dtv%name)) then
            write (unit, *, decimal='COMma', delim='quote', iostat=iostat, &
                    iomsg=iomsg) dtv%name
        else
            write (unit, *, decimal='COMma', delim='quote', iostat=iostat, &
                    iomsg=iomsg) 'name is not associated'
        end if
    end subroutine

    type(base(20,4,4)) function genBase (id, j)
        integer, intent(in) :: id, j

        genBase%id = id

        allocate (genBase%data(j), source=(/(k*1.0,k=1,j)/))

        allocate (character(10) :: genBase%name)

        write (genBase%name, '(a,i3.3)') 'xlftest', j
    end function

    class(base(:,4,4)) function genBaseAlloc(id, r)
        integer, intent(in) :: id
        real, intent(in) :: r(:)

        allocatable genBaseAlloc

        allocate (base(20,4,4) :: genBaseAlloc)

        genBaseAlloc%id = id

        allocate (genBaseAlloc%data(size(r)), source=r)

        allocate (genBaseAlloc%name, source = &
                'xlftest'//achar(iachar('0') + mod(size(r),10)))
    end function
end module

program dcmlChildWrite009a
use m
    write (1, *, decimal='Comma') (/(genBase(i, 2*i), i=1, 10)/)
    write (1, '(10DT)') (/(genBaseAlloc(i, (/(j*1.0, j=1,2*i)/)), i=1, 10)/)
end

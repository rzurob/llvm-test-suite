!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 24/07/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with READ statement
!*                             
!*
!*  DESCRIPTION                : 
!*           test round specified in subroutine for complex(16) with read.
!*           10.6.1.2.6. The rounding mode can be specified by a data
!*           transfer input/output statement, an OPEN statement or an
!*           edit descriptor.
!* ===================================================================

  program roundX16ReadEdit02 
    implicit none

    interface
      subroutine readDataFormatF(unit, data)
         integer, intent(in) :: unit
         complex(16), intent(inout):: data(6)
      end subroutine
    end interface

    integer i
    character(:), pointer :: r_mode(:) 
    complex(16) rd(6)
    real(16) vd1(6), vd2(6)

    integer, parameter::unit_r = 2 

    rd = (0.0Q0, 0.0Q0)

    allocate(r_mode(6), source=(/'up                ',         &
                                 'down              ',         &
                                 'zero              ',         &
                                 'nearest           ',         &
                                 'processor_defined ',         &
                                 'compatible        '/))

    vd1 = (/ z'3FF400355F73E1C2BC97B092CC929AFF',                    &
             z'3FF400355F73E1C13CA427B699B6B27F',                    & 
             z'3FF400355F73E1C13CA427B699B6B27F',                    &
             z'3FF400355F73E1C2BC97B092CC929B00',                    &
             z'3FF400355F73E1C2BC97B092CC929B00',                    &
             z'3FF400355F73E1C2BC97B092CC929B00'/)


    vd2 = (/z'BFF400355F73E1C23C97B092CC929AFF',                     &
            z'BFF400355F73E1C1BCA427B699B6B27F',                     &
            z'BFF400355F73E1C1BCA427B699B6B27F',                     &
            z'BFF400355F73E1C23C97B092CC929B00',                     &
            z'BFF400355F73E1C23C97B092CC929B00',                     &
            z'BFF400355F73E1C23C97B092CC929B00'/)


    open(unit_r, file='roundX16ReadEdit01.dat', action='read')

    call readDataFormatF(unit_r, rd)

    close(unit_r)

    do i =1,6
       if(qreal(rd(i)) .ne. vd1(i)) call zzrc(i)
       if(qimag(rd(i)) .ne. vd2(i)) call zzrc(i+10_4)
   end do

  end program roundX16ReadEdit02 

  subroutine readDataFormatF(unit, data)
       integer, intent(in) :: unit
       complex(16), intent(inout):: data(6)
       character(:), pointer :: r_mode(:)
       allocate(r_mode(6), source=(/'up                ',         &
                                    'down              ',         &
                                    'zero              ',         &
                                    'nearest           ',         &
                                    'processor_defined ',         &
                                    'compatible        '/))
       do i =1,6
          read(unit, '(2f26.24)', round=r_mode(i)) data(i)
          rewind(unit)
       end do

   end subroutine

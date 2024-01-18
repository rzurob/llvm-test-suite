!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with different data edit descriptor.
!*
!*  DESCRIPTION                :
!*    test different ROUND mode with different data edit descriptor with
!*    complex(16). Data trasnfer happens inside subroutine.
!*    9.4.1  The modes of a connection to an external file may be changed
!*           by a subsequent OPEN statement that modifies the connection.
!* ===================================================================

  program roundX16WriteEdit02

    implicit none

    complex(16) w1, w2

    integer, parameter::unit = 2

    open(unit, file='roundX16WriteEdit02.out', action='write')

    w1 = (1.2500586510356702302302302345691Q0,                         &
      & -1.2500586510356702302302302345691Q0)
    w2 = (3.14159265358959000230345532133343Q0,                        &
      &  2.71828182845745223034878729912374Q0)

    call writeDataFormatF(unit, w1, w2)

    call writeDataFormatOthers(unit, w1, w2)

    close(unit)

  end program roundX16WriteEdit02

  subroutine writeDataFormatF(unit, data1, data2)
       integer, intent(in) :: unit
       complex(16) data1, data2
       character(:), pointer :: r_mode(:)
       allocate(r_mode(6), source=(/'up                ',         &
                                    'down              ',         &
                                    'zero              ',         &
                                    'nearest           ',         &
                                    'processor_defined ',         &
                                    'compatible        '/))
       do i =1,6
          write(unit, '(1x,2f31.28, 1x, 2f25.23)', round=r_mode(i)) &
           & data1, data2
       end do
   end subroutine

  subroutine writeDataFormatOthers(unit, data1, data2)
       integer, intent(in) :: unit
       complex(16) data1, data2
       character(:), pointer :: r_mode(:)
       allocate(r_mode(6), source=(/'up                ',         &
                                    'down              ',         &
                                    'zero              ',         &
                                    'nearest           ',         &
                                    'processor_defined ',         &
                                    'compatible        '/))
       do i =1,6

         write(unit, '(1x, 2en35.28, 1x, 2es35.23, 1x, 2g35.29, 1x, &
          & 2d35.24, 1x,2e35.29)', round=r_mode(i)) data1, data2, data1, &
            data2, data1

       end do
   end subroutine


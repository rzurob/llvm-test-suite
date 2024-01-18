! %START
! %MAIN: YES     
! %PRECMD: rm -f fort.*  
! %COMPOPTS: 
! %GROUP: fxstio021.f 
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!--===================================================================
!  AIX XL FORTRAN V5R1 Test Case            IBM Internal Use Only
!--===================================================================
!*
!*  TEST CASE TITLE           : fxstio021.f
!*
!*  PROGAMMER                 : Catherine Sun 
!*
!*  Creation Date             : March 18, 2003 
!*
!*  Primary Function Tested   : Unformatted stream access I/O 
!*
!*  Description               : Test position of storage units of a file
!*                              opened for unformatted access with 
!*                              writing and reading different positions  
!*                              using POS specifier.      
!*
!=======================================================================

  implicit none
  integer id(10)
  integer i, j, k, l
  integer A(3,3)
  integer arr1(3), arr2(3), arr3(3), arr10(9)
  integer Acol1(3), Acol2(3), Acol3(3)
  integer int /10/
  integer, parameter :: para=100
  integer, allocatable :: hol 

  integer*1 A1(3,3)
  integer*1 arr11(3), arr12(3), arr13(3), arr110(9)
  integer*1 Acol11(3), Acol12(3), Acol13(3)

  integer*2 A2(3,3)
  integer*2 arr21(3), arr22(3), arr23(3), arr210(9)
  integer*2 Acol21(3), Acol22(3), Acol23(3)

  integer*4 A4(3,3)
  integer*4 arr41(3), arr42(3), arr43(3), arr410(9)
  integer*4 Acol41(3), Acol42(3), Acol43(3)
  integer*4 int4 /10/
  integer*4, parameter :: para4=100
  integer*4, allocatable :: hol4 
  integer*4, pointer :: int_ptr 
  integer*4, target  :: int_tar /10/
  integer*4 :: int_pointee 


  integer*8 A8(3,3)
  integer*8 arr81(3), arr82(3), arr83(3), arr810(9)
  integer*8 Acol81(3), Acol82(3), Acol83(3)

  pointer(int_pointer, int_pointee)

  allocate(hol,hol4)
  hol = 100
  hol4 = 100

  int_ptr => int_tar

  int_pointer = loc(hol4)

 ! ------------------- Integer Testing --------------------
  do i=1,3
    do j=1,3
       A(i,j) = i * 100 + j * 10
    end do 
  end do

  ! open 4 synchronous stream I/O 
  open(1, form='unformatted', access='stream', err=999)
  open(2, form='unformatted', access='stream', err=999)
  open(3, form='unformatted', access='stream', err=999)
  open(4, form='unformatted', access='stream', err=999)

  Acol1 = (/(A(i,1),i=1,3)/)
  Acol2 = (/(A(i,2),i=1,3)/)
  Acol3 = (/(A(i,3),i=1,3)/)

  write (1) a                  
  write (2, pos=int) &
 & Acol1(1), Acol1(2), Acol1(3)                     !write col 1 to unit 2
  write (3, pos=para ) (Acol2(j), j=1,3)            !write col 2 to unit 3
  write (4, pos=hol) Acol3               !write col 3 to unit 4
  
  rewind(1)
  rewind(2)
  rewind(3)
  rewind(4)

  read(1) arr10
  read(2, pos=int) & 
 & arr1(1), arr1(2), arr1(3)              !read col 1 into arr1
  read(3, pos=para) arr2           !read col 2 into arr2
  read(4, pos=hol) arr3    !read col 3 into arr3

  do i = 1 , 9
     select case(i)
        case (:3)
           if ((arr10(i) .ne. A(i,1)) .and. &
           &  (arr1(i) .ne. A(i,1))) call zzrc(i)
        case (4:6)
           if ((arr10(i) .ne. A(i-3,2)) .and. &
           &  (arr2(i-3) .ne. A(i-3,2))) call zzrc(i)
        case (7:9)
           if ((arr10(i) .ne. A(i-6,3)) .and. & 
           &  (arr3(i-6) .ne. A(i-6,3))) call zzrc(i)
     end select
  end do
 
  close(1)
  close(2)
  close(3)
  close(4)
! ---------------------- End of Integer Testing ----------------------

! ---------------------- Integer*1 Testing ----------------------

  do i=1,3
    do j=1,3
       A1(i,j) = i * 10 + j
    end do
  end do

  ! open 4 synchronous stream I/O 
  open(1, form='unformatted', access='stream', err=999)
  open(2, form='unformatted', access='stream', err=999)
  open(3, form='unformatted', access='stream', err=999)
  open(4, form='unformatted', access='stream', err=999)

  Acol11 = (/(A1(i,1),i=1,3)/)
  Acol12 = (/(A1(i,2),i=1,3)/)
  Acol13 = (/(A1(i,3),i=1,3)/)

  write (1) A1
  write (2, pos=int4) &
 & Acol11(1), Acol11(2), Acol11(3)                     !write col 1 to unit 2
  write (3, pos=para4 ) (Acol12(j), j=1,3)            !write col 2 to unit 3
  write (4, pos=hol4) Acol13               !write col 3 to unit 4

  rewind(1)
  rewind(2)
  rewind(3)
  rewind(4)

  read(1) arr110
  read(2, pos=int4) &
 & arr11(1), arr11(2), arr11(3)              !read col 1 into arr1
  read(3, pos=para4) arr12           !read col 2 into arr2
  read(4, pos=hol4) arr13    !read col 3 into arr3

  do i = 1 , 9
     select case(i)
        case (:3)
           if ((arr110(i) .ne. A1(i,1)) .and. &
           &  (arr11(i) .ne. A1(i,1))) call zzrc(i+20)
        case (4:6)
           if ((arr110(i) .ne. A1(i-3,2)) .and. &
           &  (arr12(i-3) .ne. A1(i-3,2))) call zzrc(i+20)
        case (7:9)
           if ((arr110(i) .ne. A1(i-6,3)) .and. &
           &  (arr13(i-6) .ne. A1(i-6,3))) call zzrc(i+20)
     end select
  end do

  close(1)
  close(2)
  close(3)
  close(4)
! ---------------------- End of Integer*1 Testing ----------------------

! ------------------------- Integer*2 Testing -------------------------- 

  do i=1,3
    do j=1,3
       A2(i,j) = i * 10 + j
    end do
  end do

  ! open 2 synchronous stream I/O and 2 asynchronous stream I/O
  open(1, form='unformatted', access='stream', err=999)
  open(2, form='unformatted', access='stream', err=999, asynch='yes')
  open(3, form='unformatted', access='stream', err=999)
  open(4, form='unformatted', access='stream', err=999, asynch='yes')

  Acol21 = (/(A2(i,1),i=1,3)/)
  Acol22 = (/(A2(i,2),i=1,3)/)
  Acol23 = (/(A2(i,3),i=1,3)/)

  write (1) A2
  write (2, pos=int4, id=id(1)) &
 & Acol21(1), Acol21(2), Acol21(3)                     !write col 1 to unit 2
  write (3, pos=para4 ) (Acol22(j), j=1,3)            !write col 2 to unit 3
  write (4, pos=hol4,  id=id(2)) Acol23               !write col 3 to unit 4

!  wait(id = id(1))
!  wait(id = id(2))

  rewind(1)
  read(1) arr210
  WAIT(id =id(1))

  read(2, pos=int4, id=id(3)) &
 & arr21(1), arr21(2), arr21(3)              !read col 1 into arr1

  read(3, pos=para4) arr22           !read col 2 into arr2
  wait(id=id(2))

  read(4, pos=hol4, id=id(4)) arr23    !read col 3 into arr3

  wait(id=id(3))
  wait(id=id(4))

  do i = 1 , 9
     select case(i)
        case (:3)
           if ((arr210(i) .ne. A2(i,1)) .and. &
           &  (arr21(i) .ne. A2(i,1))) call zzrc(i+40)
        case (4:6)
           if ((arr210(i) .ne. A2(i-3,2)) .and. &
           &  (arr22(i-3) .ne. A2(i-3,2))) call zzrc(i+40)
        case (7:9)
           if ((arr210(i) .ne. A2(i-6,3)) .and. &
           &  (arr23(i-6) .ne. A2(i-6,3))) call zzrc(i+40)
     end select
  end do

  close(1)
  close(2)
  close(3)
  close(4)
! ---------------------- End of Integer*2 Testing ----------------------

! -------------------------- Integer*4 Testing -------------------------

  do i=1,3
    do j=1,3
       A4(i,j) = i * 10 + j
    end do
  end do

  ! open 2 synchronous stream I/O and 2 asynchronous stream I/O
  open(1, form='unformatted', access='stream', err=999)
  open(2, form='unformatted', access='stream', asynch='yes', err=999)
  open(3, form='unformatted', access='stream', err=999)
  open(4, form='unformatted', access='stream', asynch='yes', err=999)

  Acol41 = (/(A4(i,1),i=1,3)/)
  Acol42 = (/(A4(i,2),i=1,3)/)
  Acol43 = (/(A4(i,3),i=1,3)/)

  write (1) A4
  write (2, pos=int4, id=id(1)) &
 & Acol41(1), Acol41(2), Acol41(3)                     !write col 1 to unit 2
  write (3, pos=para4 ) (Acol42(j), j=1,3)            !write col 2 to unit 3
  write (4, pos=hol4,  id=id(2)) Acol43               !write col 3 to unit 4

  rewind(1)
  read(1) arr410
  WAIT(id =id(1))
  read(2, pos=int4, id=id(3)) &
 & arr41(1), arr41(2), arr41(3)              !read col 1 into arr1
  read(3, pos=para4) arr42           !read col 2 into arr2
  wait(id=id(2))
  read(4, pos=hol4, id=id(4)) arr43    !read col 3 into arr3

  wait(id=id(3))
  wait(id=id(4))

  do i = 1 , 9
     select case(i)
        case (:3)
           if ((arr410(i) .ne. A4(i,1)) .and. &
           &  (arr41(i) .ne. A4(i,1))) call zzrc(i+60)
        case (4:6)
           if ((arr410(i) .ne. A4(i-3,2)) .and. &
           &  (arr42(i-3) .ne. A4(i-3,2))) call zzrc(i+60)
        case (7:9)
           if ((arr410(i) .ne. A4(i-6,3)) .and. &
           &  (arr43(i-6) .ne. A4(i-6,3))) call zzrc(i+60)
     end select
  end do

  close(1)
  close(2)
  close(3)
  close(4)
! ---------------------- End of Integer*4 Testing ----------------------

! -------------------------- Integer*8 Testing -------------------------

  do i=1,3
    do j=1,3
       A8(i,j) = i * 10 + j
    end do
  end do

  ! open 2 synchronous stream I/O and 2 asynchronous stream I/O
  open(1, form='unformatted', access='stream', err=999)
  open(2, form='unformatted', access='stream', asynch='yes', err=999)
  open(3, form='unformatted', access='stream', err=999)
  open(4, form='unformatted', access='stream', asynch='yes', err=999)

  Acol81 = (/(A8(i,1),i=1,3)/)
  Acol82 = (/(A8(i,2),i=1,3)/)
  Acol83 = (/(A8(i,3),i=1,3)/)

  write (1) A8
  write (2, id=id(1), pos=int_tar) &
 & Acol81(1), Acol81(2), Acol81(3)                     !write col 1 to unit 2
  write (3, pos=para4 ) (Acol82(j), j=1,3)            !write col 2 to unit 3
  write (4, pos=hol4,  id=id(2)) Acol83               !write col 3 to unit 4

  rewind(1)
  read(1) arr810
  WAIT(id =id(1))

  read(2, pos=int_ptr, id=id(3)) &
 & arr81(1), arr81(2), arr81(3)              !read col 1 into arr1
  read(3, pos=para4) arr82           !read col 2 into arr2

  wait(id=id(2))
  read(4, pos=int_pointee, id=id(4)) arr83    !read col 3 into arr3

  wait(id=id(3))
  wait(id=id(4))

  do i = 1 , 9
     select case(i)
        case (:3)
           if ((arr810(i) .ne. A8(i,1)) .and. &
           &  (arr81(i) .ne. A8(i,1))) call zzrc(i+80)
        case (4:6)
           if ((arr810(i) .ne. A8(i-3,2)) .and. &
           &  (arr82(i-3) .ne. A8(i-3,2))) call zzrc(i+80)
        case (7:9)
           if ((arr810(i) .ne. A8(i-6,3)) .and. &
           &  (arr83(i-6) .ne. A8(i-6,3))) call zzrc(i+80)
     end select
  end do

  close(1)
  close(2)
  close(3)
  close(4)
! ---------------------- End of Integer*8 Testing ----------------------
  goto 900
999 error stop 101
900 continue
end

! *********************************************************************
!*  ===================================================================
!*
!*  Creation Date              : Mar 07, 2003
!*
!*  Primary Function Tested    : Unformatted stream access I/O
!*
!*  Description                : Test record structure(scalar and
!*                               array) with Stream I/O.
!*
!=======================================================================

  implicit none

  integer ios, i, j, caseid

  structure /dt1/
    integer   int
    real      r
    complex   comp
    character char
    byte      bt
    logical   log
  end structure

  structure /dt2/
    integer   inta(3)
    real      ra(3)
    complex   compa(3)
    character chara(3)
    byte      bta(3)
    logical   loga(3)
  end structure

  structure /dt3/
    integer   inta2(2,2)
    real      ra2(2,2)
    complex   compa2(2,2)
    character chara2(2,2)
    byte      bta2(2,2)
    logical   loga2(2,2)
  end structure

  record/dt1/ a1, b1
  record/dt2/ a2, b2
  record/dt3/ a3, b3

  a1 = dt1(100, 1.0, (1.0, 2.0), "abc", &
    "A", .true.)

  a2 = dt2((/1, 0, -10/), (/100.0, 0.0, -1.0/),  &
    (/(20.0e1, 30.0e1), (0.0e0, 0.0e0), (-21.0e0, -31.0e0)/), &
    (/"A", " ", "m"/), (/"b", "1", "."/),  &
    (/.false.,  .true., .false./))

  a3 = dt3(reshape((/a2%inta(1), a2%inta(2),a2%inta(3), a1%int/), &
    (/2,2/)), reshape((/a2%ra(1), a2%ra(2), a2%ra(3), a1%r/), &
    (/2,2/)), reshape((/a2%compa(1), a2%compa(2), a2%compa(3), a1%comp/), &
    (/2,2/)), reshape((/a2%chara(1), a2%chara(2), a2%chara(3), a1%char/), &
    (/2,2/)), reshape((/a2%bta(1), a2%bta(2), a2%bta(3), a1%bt/), &
    (/2,2/)), reshape((/a2%loga(1), a2%loga(2), a2%loga(3), a1%log/), &
    (/2,2/)))

!* open three unformatted stream units for synchronous I/O

  open(1, form='unformatted', access='stream', iostat=ios, err=100)
  open(2, form='unformatted', access='stream', iostat=ios, err=100)
  open(3, form='unformatted', access='stream', iostat=ios, err=100)

  write (1, iostat=ios, err=200) a1
  write (2, iostat=ios, err=200) a2
  write (3, iostat=ios, err=200) a3

!* Test dt1
  rewind(1, iostat=ios, err=500)
  read (1,iostat=ios, err=400) b1
  caseid =10
  if( a1%int .ne. b1%int )                   call zzrc(caseid+1)
  if( a1%r .ne. b1%r )                       call zzrc(caseid+2)
  if( a1%comp .ne. b1%comp )                 call zzrc(caseid+3)
  if( a1%char .ne. b1%char )                 call zzrc(caseid+4)
  if( a1%bt .ne. b1%bt )                     call zzrc(caseid+5)
  if( a1%log .neqv. b1%log )                 call zzrc(caseid+6)

  close(1, status='delete')

!* Test dt2
  rewind(2, iostat=ios, err=500)
  read(2, iostat=ios, err=400) b2
  caseid =20

  do i = 1, 3
     if(a2%inta(i) .ne. b2%inta(i))          call zzrc(i*caseid+1)
     if(a2%ra(i) .ne. b2%ra(i))              call zzrc(i*caseid+2)
     if(a2%compa(i) .ne. b2%compa(i))        call zzrc(i*caseid+3)
     if(a2%chara(i) .ne. b2%chara(i))        call zzrc(i*caseid+4)
     if(a2%bta(i) .ne. b2%bta(i))            call zzrc(i*caseid+5)
     if(a2%loga(i) .neqv. b2%loga(i))        call zzrc(i*caseid+6)
  enddo

  close(2, status='delete')

!* Test dt3

  rewind(3, iostat=ios, err=500)
  read(3, iostat=ios, err=400) b3
  caseid =100

  do i=1, 2
     do j=1, 2
        if (a3%inta2(i,j) .ne. b3%inta2(i,j)) &
         call zzrc(caseid+i*j+1)
        if (a3%ra2(i,j) .ne. b3%ra2(i,j)) &
         call zzrc(caseid+i*j+2)
        if (a3%compa2(i,j) .ne. b3%compa2(i,j)) &
         call zzrc(caseid+i*j+3)
        if (a3%chara2(i,j) .ne. b3%chara2(i,j)) &
         call zzrc(caseid+i*j+4)
        if (a3%bta2(i,j) .ne. b3%bta2(i,j)) &
         call zzrc(caseid+i*j+5)
        if (a3%loga2(i,j) .neqv. b3%loga2(i,j)) &
         call zzrc(caseid+i*j+6)
     enddo
  enddo

  close(3, status='delete')

stop

100 print *, "open error: iostat = ", ios
    error stop 100
200 print *, "write error: iostat = ", ios
    error stop 200
300 print *, "inquire error: iostat = ", ios
    error stop 300
400 print *, "read error: iostat = ", ios
    error stop 400
500 print *, "rewind error: iostat = ", ios
    error stop 500
end

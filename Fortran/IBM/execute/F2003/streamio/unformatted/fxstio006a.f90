! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:  rm -f fort.*
! %COMPOPTS:
! %GROUP: fxstio006a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  Creation Date              : Mar 07, 2003
!*
!*  Primary Function Tested    : Ununformatted stream access I/O
!*
!*  Description                : Test F90  pointers with
!*                               Unformatted Synchronous Stream I/O.
!*
!=======================================================================

   include 'array_check.h'

      implicit none

     integer ios

!* F90 Declaration
     logical cmp_x8_array,cmp_x16_array,cmp_x32_array
     logical cmp_r4_array,cmp_r8_array,cmp_r16_array
     logical cmp_i1_array,cmp_i2_array,cmp_i4_array,cmp_i8_array
     logical cmp_l1_array,cmp_l2_array,cmp_l4_array,cmp_l8_array
     logical cmp_b_array,cmp_ch_array

      complex   , pointer :: p_xar1(:)
      real(16), pointer :: p_r16ar2(:,:)
      integer(4), pointer :: p_i4var
      logical  , pointer :: p_lar1(:)
      character*3, pointer :: p_ch4ar2(:,:)
      byte, pointer :: p_b4var

      complex   , target :: t_xar1(3) = (/(1.0,-1.0),(2.0,-2.0),(3.0,-3.0)/)
      real(16), target :: t_r16ar2(-3:-1,2:3) &
         = reshape((/1.0q2,2.0q2,3.0q2,4.0q2,5.0q2,6.0q2/),(/3,2/))
      integer*4, target :: t_i4var = 12345678
      logical  , target :: t_lar1(2:4) = (/.true.,.false.,.true./)
      character*3, target :: t_ch4ar2(0:1,1:3)&
         = reshape((/'a12','b12','c12','d12','e12','f12'/),(/2,3/))
      byte, target :: t_b4var = .true.

!* Associate pointers

    p_xar1 => t_xar1
    p_r16ar2 => t_r16ar2
    p_i4var => t_i4var
    p_lar1 => t_lar1
    p_ch4ar2 => t_ch4ar2
    p_b4var => t_b4var

!* start of tests

  open(1, access='stream', form='unformatted', action='readwrite', &
     iostat=ios, err=100)
   write(1, iostat=ios, err=200) p_xar1
   rewind(1, iostat=ios, err=500)
!  print *, t_xar1
!  print *, cmp_x8_array(t_xar1,(/(1.0,-1.0),(2.0,-2.0),(3.0,-3.0)/),3)
   if(cmp_x8_array(t_xar1,(/(1.0,-1.0),(2.0,-2.0),(3.0,-3.0)/),3) &
     .neqv. .true.)  error stop 111

  open(2, access='stream', form='unformatted', action='readwrite', &
     iostat=ios, err=100)
  write(2, iostat=ios, err=200) p_r16ar2
  rewind(2, iostat=ios, err=500)
  if(cmp_r16_array(t_r16ar2,(/1.0q2,2.0q2,3.0q2,4.0q2,5.0q2,6.0q2/),&
     6) .neqv. .true.)  error stop 2

  open(3, access='stream', form='unformatted', action='readwrite', &
     iostat=ios, err=100)
  write(3, iostat=ios, err=200) p_i4var
  rewind(3, iostat=ios, err=500)
  if(t_i4var .ne. 12345678) error stop 3

  open(4, access='stream', form='unformatted', action='readwrite', &
     iostat=ios, err=100)
  write(4, iostat=ios, err=200) p_lar1
  rewind(4, iostat=ios, err=500)
  if(cmp_l4_array(t_lar1,(/.true.,.false.,.true./),3) .neqv. .true.)  &
     error stop 4

  open(7, access='stream', form='unformatted', action='readwrite', &
     iostat=ios, err=100)
  write(7, iostat=ios, err=200) p_ch4ar2
  rewind(7, iostat=ios, err=500)
  if(cmp_ch_array(t_ch4ar2,(/'a12','b12','c12','d12','e12','f12'/),&
     6) .neqv. .true.)   error stop 5

  open(8, access='stream', form='unformatted', action='readwrite', &
     iostat=ios, err=100)
  write(8, iostat=ios, err=200) p_b4var
  rewind(8, iostat=ios, err=500)
  if(t_b4var .neqv. .true.) error stop 6

!* set the targets to some other values

   t_xar1 = (/(0.0,-0.0),(0.0,-0.0),(0.0,-0.0)/)
   t_r16ar2 = reshape((/0.0q0,0.0q0,0.0q0,0.0q0,0.0q0,0.0q0/),(/3,2/))
   t_i4var = 0
   t_lar1 = (/.false.,.true.,.false./)
   t_ch4ar2 = reshape((/'000','000','000','000','000','000'/),(/2,3/))
   t_b4var = .false.

!* Read values back

   read(1, iostat=ios, err=400) p_xar1
   if(cmp_x8_array(t_xar1,(/(1.0,-1.0),(2.0,-2.0),(3.0,-3.0)/),3) &
     .neqv. .true.)  error stop 7
   close(1, status='delete')

   read(2, iostat=ios, err=400) p_r16ar2
   if(cmp_r16_array(t_r16ar2,(/1.0q2,2.0q2,3.0q2,4.0q2,5.0q2,6.0q2/),&
     6) .neqv. .true.)  error stop 8
   close(2, status='delete')

   read(3, iostat=ios, err=400) p_i4var
   if(t_i4var .ne. 12345678) error stop 9
   close(3, status='delete')

   read(4, iostat=ios, err=400) p_lar1
   if(cmp_l4_array(t_lar1,(/.true.,.false.,.true./),3) .neqv. .true.)  &
     error stop 10
   close(4, status='delete')

   read(7, iostat=ios, err=400) p_ch4ar2
   if(cmp_ch_array(t_ch4ar2,(/'a12','b12','c12','d12','e12','f12'/),&
     6) .neqv. .true.) error stop 11
   close(7, status='delete')

   read(8, iostat=ios, err=400) p_b4var
   if(t_b4var .neqv. .true.) error stop 12
   close(8, status='delete')
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

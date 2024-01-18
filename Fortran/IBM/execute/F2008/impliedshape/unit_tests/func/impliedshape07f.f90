!* =================================================================== &
!*
!* DATE                       : February 14, 2011
!* ORIGIN                     : AIX Compiler Development,
!*
!* PRIMARY FUNCTIONS TESTED   : Implied-shape arrays
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              implied-shape arrays with RHS array
!*                              sections
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program impliedshape07f

      type base
        integer      :: a = 1
        character(4) :: b = 'abcd'
      end type

      integer, parameter :: i1 (*) = [    (i,i=10,90)    ]
      integer, parameter :: i2 (*,*) = reshape([(i,i=101,200)],[10,10])
      integer, parameter :: i3 (*,*,*) = &
           & reshape([(i,i=601,1600)],[10,10,10])
      integer, parameter :: i4 (*,*,*,*) = &
           & reshape([(i,i=9001,12000)],[10,10,10,3])

      real, parameter :: r1 (*) = [ (i,i=1,10) ]
      real, parameter :: r2 (*,*) = reshape([(i,i=101,200)],[10,10])
      real, parameter :: r3 (*,*,*) = &
           & reshape([(i,i=601,1600)],[10,10,10])
      real, parameter :: r4 (*,*,*,*) = &
           & reshape([(i,i=9001,12000)],[10,10,10,2])

      complex, parameter :: c1 (10) = [ ((i,i+30),i=1,10) ]
      complex, parameter :: c2 (*,*) = reshape([((i,i),i=101,200)],[10,10])
      complex, parameter :: c3 (*,*,*) = &
           & reshape([((i,i),i=601,1600)],[10,10,10])
      complex, parameter :: c4 (*,*,*,*) = &
           & reshape([((i,i),i=9001,12000)],[10,10,10,2])

      character(*), parameter :: ch1 (*) = [ ((CHAR(i)),i=30,40) ]
      character(*), parameter :: ch2 (*,*) = &
           & reshape([(CHAR(48+(MOD(i,10)))//CHAR(48+(MOD(i,10))),i=0,99)],[10,10])
      character(*), parameter :: ch3 (*,*,*) = &
           & reshape([('zzzzzz',i=601,1600)],[10,10,10])
      character(*), parameter :: ch4 (*,*,*,*) = &
           & reshape([('aaa',i=9001,12000)],[10,10,10,2])

      type(base), parameter :: t1 (*) = reshape([base(), base(10,'XLF ')],[2])
      type(base), parameter :: t2 (*,*) = &
           & reshape([(base(i,'IJKL'),i=101,200)],[10,10])
      type(base), parameter :: t3 (*,*,*) = &
           & reshape([(base(i,'EFGH'),i=601,1600)],[10,10,10])
      type(base), parameter :: t4 (*,*,*,*) = &
           & reshape([(base(i,'ABCD'),i=9001,12000)],[10,10,10,2])


      integer, parameter :: ia (*) =   i1(1:10:2)
      integer, parameter :: ib (*,*) = i2(:,1:10:3)
      integer, parameter :: ic (*,*) = i2(1:8:4,1:10:3)
      integer, parameter :: id (*,*,*) = i3(8:10:1,:,4:6:2)
      integer, parameter :: ie (*,*,*,*) = i4(:,:,1:8:2,1:2)
      integer, parameter :: if (*) = r1(1:5:3)
      integer, parameter :: ig (*,*,*) = c4(:,:,3:4,1)
      integer, parameter :: ih (*) = t4(1,1,1,:)%a

      real, parameter :: ra (*) =   i1(1:10:2)
      real, parameter :: rb (*,*) = r2(:,1:10:3)
      real, parameter :: rc (*,*) = r2(1:8:4,1:10:3)
      real, parameter :: rd (*,*,*) = r3(8:10:1,:,4:6:2)
      real, parameter :: re (*,*,*,*) = i4(:,:,1:8:2,1:2)
      real, parameter :: rf (*) = r1(1:5:3)
      real, parameter :: rg (*,*,*) = c4(:,:,3:4,1)
      real, parameter :: rh (*) = t4(1,1,1,:)%a

      complex, parameter :: ca (*) =   r1(1:10:2)
      complex, parameter :: cb (*,*) = r2(:,1:10:3)
      complex, parameter :: cc (*,*) = c2(1:8:4,1:10:3)
      complex, parameter :: cd (*,*,*) = i3(8:10:1,:,4:6:2)
      complex, parameter :: ce (*,*,*,*) = i4(:,:,1:8:2,1:2)
      complex, parameter :: cf (*) = c1(1:5:3)
      complex, parameter :: cg (*,*,*) = c4(:,:,3:4,1)
      complex, parameter :: ch (*) = t4(1,1,1,:)%a

      character(*), parameter :: cha (*) = CHAR(48+MOD(i1(1:10:2),10))
      character(*), parameter :: chb (*,*) = CHAR(48+MOD(i2(:,1:10:3),10))
      character(*), parameter :: chc (*,*) = CHAR(48+MOD(i2(1:8:4,1:10:3),10))
      character(*), parameter :: chd (*,*,*) = CHAR(48+MOD(i3(8:10:1,:,4:6:2),10))
      character(*), parameter :: che (*,*,*,*) = CHAR(48+MOD(i4(:,:,1:8:2,1:2),10))
      character(*), parameter :: chf (*) = CHAR(48+MOD(INT(r1(1:5:3)),10))
      character(*), parameter :: chg (*,*,*) = CHAR(48+MOD(INT(c4(:,:,3:4,1)),10))
      character(*), parameter :: chh (*) = CHAR(48+MOD(t4(1,1,1,:)%a,10))

      type(base), parameter :: ta (*) =   t1(1:2:2)
      type(base), parameter :: tb (*,*) = t2(:,1:10:3)
      type(base), parameter :: tc (*,*) = t2(1:8:4,1:10:3)
      type(base), parameter :: td (*,*,*) = t3(8:10:1,:,4:6:2)
      type(base), parameter :: te (*,*,*,*) = t4(:,:,1:8:2,1:2)
      type(base), parameter :: tf (*) = t2(:,1)
      type(base), parameter :: tg (*,*,*) = t4(:,:,3:4,1)
      type(base), parameter :: th (*) = t4(1,1,1,:)

      integer, parameter :: iaz (5) =   i1(1:10:2)
      integer, parameter :: ibz (10,4) = i2(:,1:10:3)
      integer, parameter :: icz (2,4) = i2(1:8:4,1:10:3)
      integer, parameter :: idz (3,10,2) = i3(8:10:1,:,4:6:2)
      integer, parameter :: iez (10,10,4,2) = i4(:,:,1:8:2,1:2)
      integer, parameter :: ifz (2) = r1(1:5:3)
      integer, parameter :: igz (10,10,2) = c4(:,:,3:4,1)
      integer, parameter :: ihz (2) = t4(1,1,1,:)%a

      real, parameter :: raz (5) =   i1(1:10:2)
      real, parameter :: rbz (10,4) = r2(:,1:10:3)
      real, parameter :: rcz (2,4) = r2(1:8:4,1:10:3)
      real, parameter :: rdz (3,10,2) = r3(8:10:1,:,4:6:2)
      real, parameter :: rez (10,10,4,2) = i4(:,:,1:8:2,1:2)
      real, parameter :: rfz (2) = r1(1:5:3)
      real, parameter :: rgz (10,10,2) = c4(:,:,3:4,1)
      real, parameter :: rhz (2) = t4(1,1,1,:)%a

      complex, parameter :: caz (5) =   r1(1:10:2)
      complex, parameter :: cbz (10,4) = r2(:,1:10:3)
      complex, parameter :: ccz (2,4) = c2(1:8:4,1:10:3)
      complex, parameter :: cdz (3,10,2) = i3(8:10:1,:,4:6:2)
      complex, parameter :: cez (10,10,4,2) = i4(:,:,1:8:2,1:2)
      complex, parameter :: cfz (2) = c1(1:5:3)
      complex, parameter :: cgz (10,10,2) = c4(:,:,3:4,1)
      complex, parameter :: chz (2) = t4(1,1,1,:)%a

      character(*), parameter :: chaz (5) = CHAR(48+MOD(i1(1:10:2),10))
      character(*), parameter :: chbz (10,4) = CHAR(48+MOD(i2(:,1:10:3),10))
      character(*), parameter :: chcz (2,4) = CHAR(48+MOD(i2(1:8:4,1:10:3),10))
      character(*), parameter :: chdz (3,10,2) = CHAR(48+MOD(i3(8:10:1,:,4:6:2),10))
      character(*), parameter :: chez (10,10,4,2) = CHAR(48+MOD(i4(:,:,1:8:2,1:2),10))
      character(*), parameter :: chfz (2) = CHAR(48+MOD(INT(r1(1:5:3)),10))
      character(*), parameter :: chgz (10,10,2) = CHAR(48+MOD(INT(c4(:,:,3:4,1)),10))
      character(*), parameter :: chhz (2) = CHAR(48+MOD(t4(1,1,1,:)%a,10))

      type(base), parameter :: taz (1) =   t1(1:2:2)
      type(base), parameter :: tbz (10,4) = t2(:,1:10:3)
      type(base), parameter :: tcz (2,4) = t2(1:8:4,1:10:3)
      type(base), parameter :: tdz (3,10,2) = t3(8:10:1,:,4:6:2)
      type(base), parameter :: tez (10,10,4,2) = t4(:,:,1:8:2,1:2)
      type(base), parameter :: tfz (10) = t2(:,1)
      type(base), parameter :: tgz (10,10,2) = t4(:,:,3:4,1)
      type(base), parameter :: thz (2) = t4(1,1,1,:)

      if (ANY(ia .NE. iaz)) ERROR STOP 1
      if (ANY(ib .NE. ibz)) ERROR STOP 2
      if (ANY(ic .NE. icz)) ERROR STOP 3
      if (ANY(id .NE. idz)) ERROR STOP 4
      if (ANY(ie .NE. iez)) ERROR STOP 5
      if (ANY(if .NE. ifz)) ERROR STOP 6
      if (ANY(ig .NE. igz)) ERROR STOP 7
      if (ANY(ih .NE. ihz)) ERROR STOP 8

      if (ANY(ra .NE. raz)) ERROR STOP 21
      if (ANY(rb .NE. rbz)) ERROR STOP 22
      if (ANY(rc .NE. rcz)) ERROR STOP 23
      if (ANY(rd .NE. rdz)) ERROR STOP 24
      if (ANY(re .NE. rez)) ERROR STOP 25
      if (ANY(rf .NE. rfz)) ERROR STOP 26
      if (ANY(rg .NE. rgz)) ERROR STOP 27
      if (ANY(rh .NE. rhz)) ERROR STOP 28

      if (ANY(ca .NE. caz)) ERROR STOP 31
      if (ANY(cb .NE. cbz)) ERROR STOP 32
      if (ANY(cc .NE. ccz)) ERROR STOP 33
      if (ANY(cd .NE. cdz)) ERROR STOP 34
      if (ANY(ce .NE. cez)) ERROR STOP 35
      if (ANY(cf .NE. cfz)) ERROR STOP 36
      if (ANY(cg .NE. cgz)) ERROR STOP 37
      if (ANY(ch .NE. chz)) ERROR STOP 38

      if (ANY(cha .NE. chaz)) ERROR STOP 41
      if (ANY(chb .NE. chbz)) ERROR STOP 42
      if (ANY(chc .NE. chcz)) ERROR STOP 43
      if (ANY(chd .NE. chdz)) ERROR STOP 44
      if (ANY(che .NE. chez)) ERROR STOP 45
      if (ANY(chf .NE. chfz)) ERROR STOP 46
      if (ANY(chg .NE. chgz)) ERROR STOP 47
      if (ANY(chh .NE. chhz)) ERROR STOP 48

      if (ANY(ta%a .NE. taz%a) .OR. ANY(ta%b .NE. taz%b)) ERROR STOP 51
      if (ANY(tb%a .NE. tbz%a) .OR. ANY(ta%b .NE. taz%b)) ERROR STOP 52
      if (ANY(tc%a .NE. tcz%a) .OR. ANY(ta%b .NE. taz%b)) ERROR STOP 53
      if (ANY(td%a .NE. tdz%a) .OR. ANY(ta%b .NE. taz%b)) ERROR STOP 54
      if (ANY(te%a .NE. tez%a) .OR. ANY(ta%b .NE. taz%b)) ERROR STOP 55
      if (ANY(tf%a .NE. tfz%a) .OR. ANY(ta%b .NE. taz%b)) ERROR STOP 56
      if (ANY(tg%a .NE. tgz%a) .OR. ANY(ta%b .NE. taz%b)) ERROR STOP 57
      if (ANY(th%a .NE. thz%a) .OR. ANY(ta%b .NE. taz%b)) ERROR STOP 58

      end

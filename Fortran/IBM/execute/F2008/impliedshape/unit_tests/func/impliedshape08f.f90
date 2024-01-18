!* =================================================================== &
!* XL Fortran Test Case                          IBM INTERNAL USE ONLY
!* =================================================================== &
!*
!* TEST CASE TITLE            : impliedshape08f.f
!*
!* PROGRAMMER                 : David Nichols
!* DATE                       : February 14, 2011
!* ORIGIN                     : AIX Compiler Development,
!*                            : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : Implied-shape arrays
!*
!* DRIVER STANZA              : xlf2008
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              implied-shape arrays as associate
!*                              selecter
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program impliedshape08f

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


      associate ( x => ia )
        if (ANY(x .NE. iaz)) ERROR STOP 1
      end associate 
      associate ( x => ib )
        if (ANY(x .NE. ibz)) ERROR STOP 2
      end associate 
      associate ( x => ic )
        if (ANY(x .NE. icz)) ERROR STOP 3
      end associate 
      associate ( x => id )
        if (ANY(x .NE. idz)) ERROR STOP 4
      end associate 
      associate ( x => ie )
        if (ANY(x .NE. iez)) ERROR STOP 5
      end associate 
      associate ( x => if )
        if (ANY(x .NE. ifz)) ERROR STOP 6
      end associate 
      associate ( x => ig )
        if (ANY(x .NE. igz)) ERROR STOP 7
      end associate 
      associate ( x => ih )
        if (ANY(x .NE. ihz)) ERROR STOP 8
      end associate 

      associate ( x => ra )
        if (ANY(x .NE. raz)) ERROR STOP 21
      end associate 
      associate ( x => rb )
        if (ANY(x .NE. rbz)) ERROR STOP 22
      end associate 
      associate ( x => rc )
        if (ANY(x .NE. rcz)) ERROR STOP 23
      end associate 
      associate ( x => rd )
        if (ANY(x .NE. rdz)) ERROR STOP 24
      end associate 
      associate ( x => re )
        if (ANY(x .NE. rez)) ERROR STOP 25
      end associate 
      associate ( x => rf )
        if (ANY(x .NE. rfz)) ERROR STOP 26
      end associate 
      associate ( x => rg )
        if (ANY(x .NE. rgz)) ERROR STOP 27
      end associate 
      associate ( x => rh )
        if (ANY(x .NE. rhz)) ERROR STOP 28
      end associate 

      associate ( x => ca )
        if (ANY(x .NE. caz)) ERROR STOP 31
      end associate 
      associate ( x => cb )
        if (ANY(x .NE. cbz)) ERROR STOP 32
      end associate 
      associate ( x => cc )
        if (ANY(x .NE. ccz)) ERROR STOP 33
      end associate 
      associate ( x => cd )
        if (ANY(x .NE. cdz)) ERROR STOP 34
      end associate 
      associate ( x => ce )
        if (ANY(x .NE. cez)) ERROR STOP 35
      end associate 
      associate ( x => cf )
        if (ANY(x .NE. cfz)) ERROR STOP 36
      end associate 
      associate ( x => cg )
        if (ANY(x .NE. cgz)) ERROR STOP 37
      end associate 
      associate ( x => ch )
        if (ANY(x .NE. chz)) ERROR STOP 38
      end associate 

      associate ( x => cha )
        if (ANY(x .NE. chaz)) ERROR STOP 41
      end associate 
      associate ( x => chb )
        if (ANY(x .NE. chbz)) ERROR STOP 42
      end associate 
      associate ( x => chc )
        if (ANY(x .NE. chcz)) ERROR STOP 43
      end associate 
      associate ( x => chd )
        if (ANY(x .NE. chdz)) ERROR STOP 44
      end associate 
      associate ( x => che )
        if (ANY(x .NE. chez)) ERROR STOP 45
      end associate 
      associate ( x => chf )
        if (ANY(x .NE. chfz)) ERROR STOP 46
      end associate 
      associate ( x => chg )
        if (ANY(x .NE. chgz)) ERROR STOP 47
      end associate 
      associate ( x => chh )
        if (ANY(x .NE. chhz)) ERROR STOP 48
      end associate 

      associate ( x => ta )
        if (ANY(x%a .NE. taz%a) .OR. ANY(x%b .NE. taz%b)) ERROR STOP 51
      end associate 
      associate ( x => tb )
        if (ANY(x%a .NE. tbz%a) .OR. ANY(x%b .NE. tbz%b)) ERROR STOP 52
      end associate 
      associate ( x => tc )
        if (ANY(x%a .NE. tcz%a) .OR. ANY(x%b .NE. tcz%b)) ERROR STOP 53
      end associate 
      associate ( x => td )
        if (ANY(x%a .NE. tdz%a) .OR. ANY(x%b .NE. tdz%b)) ERROR STOP 54
      end associate 
      associate ( x => te )
        if (ANY(x%a .NE. tez%a) .OR. ANY(x%b .NE. tez%b)) ERROR STOP 55
      end associate 
      associate ( x => tf )
        if (ANY(x%a .NE. tfz%a) .OR. ANY(x%b .NE. tfz%b)) ERROR STOP 56
      end associate 
      associate ( x => tg )
        if (ANY(x%a .NE. tgz%a) .OR. ANY(x%b .NE. tgz%b)) ERROR STOP 57
      end associate 
      associate ( x => th )
        if (ANY(x%a .NE. thz%a) .OR. ANY(x%b .NE. thz%b)) ERROR STOP 58
      end associate 

      end

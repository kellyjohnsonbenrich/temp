      PROGRAM MAKESPLPSU
C** This program performs the same functions for PSU-WOPWOP
C** results that MAKESPL performs for WOPWOP results.  Instead
C** of reading in data from the [Name]wopout file, data is read
C** in from the "oaspldB.dat" and "spl_spectrum.dat" files created
C** by process_psuwopwop after PSU-WOPWOP runs are completed.

C** NOTE: PSU-WOPWOP outputs data at rotor harmonics
C**       WOPWOP outputs data at blade passage harmonics

C** This program generates a C** [Name]_#1-#2spl.dat file with the
C** sound pressure level from BPF harmonics #1 to #2

CDAW 6-3-2010: It now also generates a [Name]_#1-#2spltek.dat file
CDAW 6-3-2010: for creating a contour plot using Tekplot

C**  Usage:   makesplpsu [Name] #1 #2
C**            will make the file [Name]spl_#1-#2.dat with SPL from
C**            harmonics (of blade passage frequency, not rotor revs)
C              #1 to #2 found in file spl_spectrum.dat.
C              [Name}spl_#1-#2spltek.dat is file for Tecplot carpet plots

      CHARACTER*120 line,header
      CHARACTER*40 finp1,finp2,fout,ftek,fmax
      CHARACTER*2 charm
      external LENSTR
      data RE/93.979/
      data nlow,nhigh/0,-1/

C** Open the input file.

c     print*,'Remember, in PSU-WOPWOP, you were able to run in CHARM'
c     print*,'          coordinates unlike WOPWOP where Y=-Y and Z=-Z'

      write(*,998)
998   format(' Enter blade number: ',$)
      read(*,*) nblade

      write(*,999)
999   format(' Enter blade radius to be used to normalize',
     $       ' output locations: ',$)
      read(*,*) radius

      nargs = iargc()
      if (nargs.lt.2) then
       print*,' '
       print*,' Type makesplpsu [filename] [#1] [#2] to generate a file'
       print*,' [filename_[#1]-[#2]] with the SPL between harmonics'
       print*,' [#1] and [#2].  Harmonics are of blade'
       print*,' passage frequency = NBLADE times the rotor rev freq)'
       print*,' from file [filename]wopout'
       print*,' If [#2] is absent you get harmonics {#1} and above.'
       print*,' If [#1] is absent you get all harmonics possible.'
       print*,'  '
      end if

1000  FORMAT(A)

C** Assign the input filenames
      finp1='oaspldB.dat'
      finp2='spl_spectrum.dat'

C** Assign the output filename
      CALL GETARG(1,fout)
      len=lenstr(fout)
      if (nargs.gt.1) then
       fout = fout(1:len)//'_'
       len = len + 1
       CALL GETARG(2,charm)
       lenh = lenstr(charm)
       if (lenh.eq.1) read(charm,1001) nlow
       if (lenh.eq.2) read(charm,1002) nlow
       if (lenh.eq.3) read(charm,1003) nlow
1001   format(i1)
1002   format(i2)
1003   format(i3)
       fout = fout(1:len)//charm(1:lenh)
       len = lenstr(fout)
       fout = fout(1:len)//'-'
       len = len + 1
       if (nargs.eq.3) then
        CALL GETARG(3,charm)
        lenh = lenstr(charm)
        if (lenh.eq.1) read(charm,1001) nhigh
        if (lenh.eq.2) read(charm,1002) nhigh
        if (lenh.eq.3) read(charm,1003) nhigh
        fout = fout(1:len)//charm(1:lenh)
        len=lenstr(fout)
       end if
      end if
      fout = fout(1:len)//'spl.dat'
      len=lenstr(fout)

CDAW 6-3-2010: Tekplot support
      len = lenstr(fout)
      ftek = fout
      ftek(len-3:len+3) = "tek.dat"

C** Ouput the peak SPL location and magnitude
      fmax = fout
      fmax(len-3:len+3) = "max.dat"
      advmax = 0.0
      retmax = 0.0

      if (nargs.le.2) WRITE(*,1004) finp1,finp2,fout,ftek,nlow
      if (nargs.eq.3) WRITE(*,1005) finp1,finp2,fout,ftek,nlow,nhigh
1004  format('  Input filename: ',A/
     $       '  Input filename: ',A/
     $       ' Output filename: ',A/
     $       ' Output filename: ',A/
     $       ' Harmonic range selected is: ',I2,' and above'/)
1005  format('  Input filename: ',A/
     $       '  Input filename: ',A/
     $       ' Output filename: ',A/
     $       ' Output filename: ',A/
     $       ' Harmonic range selected is: ',I2,' to ',I2/)
      nin1 = 99
      nout = 98
      ntek = 97
      nmax = 96
      nin2 = 95
      OPEN(nin1,FILE=finp1,STATUS='OLD')
      OPEN(nin2,FILE=finp2,STATUS='OLD')
      OPEN(nout,FILE=fout,STATUS='UNKNOWN')
      OPEN(ntek,FILE=ftek,STATUS='UNKNOWN')
      OPEN(nmax,FILE=fmax,STATUS='UNKNOWN')
      write(*,1006)
1006  format(' Enter NX,NY,NZ: ',$)
      read(*,*) NX,NY,NZ
      write(nout,1007)
1007  format(3X,'observer',5X,'x',11X,'y',12X,'z',7X,'Thickness',4X,
     $       'Loading',5X,'OASPLdB',A1)

CDAW 6-3-2010 Tekplot support
      write(ntek,*) 'VARIABLES = "X","Y","Z",
     $"Thickness","Loading","Overall"'
      write(ntek,*) 'ZONE T="0.0"'
      write(ntek,*) 'I=',NX,'  J=',NY,'  K=',NZ,'  F=POINT'

      READ(nin1,1000) LINE   ! header oaspldB.dat file
      READ(nin2,1000) LINE   ! header spl_spectrum.dat file
      READ(LINE(101:105),1008) mharm
1008  FORMAT(I5)
      nharm = mharm/nblade
      if (nhigh.eq.-1) nhigh = nharm   ! if no entry, set to max
      if (nhigh.gt.nharm) then
       print*,'You requested output up to BPF = ',nhigh
       print*,'No can do.'
       stop
      end if
      mlow = nlow*4     ! convert from rotor freq to BPF
      mhigh = nhigh*4   ! convert from rotor freq to BPF
      
C** Read in the OBSERVER position 
5     READ(nin1,*,END=100) nobs,X,Y,Z    ! observer location
cccccc convert to X/R
      X = X/radius
      Y = Y/radius
      Z = Z/radius

C** Process the spectral information for this observer
C** Skip harmonics below nlow.
C** Unlike WOPWOP, nlow=0 is now different from nlow=1 because
C** PSU-WOPWOP includes goes down to 0.0

C**  Convert amp from dB to PA (using reverse formula from SUBROUTINE SPCTRA
C**  in WOPWOP, hence the 1.E-20 term.)
      sumT = 0.0
      sumL = 0.0
      sumO = 0.0

      do i=1,mharm
       read (nin2,1000) line
       if (i.ge.mlow .and. i.le.mhigh) then
        READ(LINE(18:24),1030) ampT  ! Thickness
        READ(LINE(30:36),1030) ampL  ! Loading
        READ(LINE(42:48),1030) ampO  ! Overall
1030    FORMAT(F6.2)
        ampT = 10.0**( (ampT-RE)/20.0 ) - 1.E-20
        ampL = 10.0**( (ampL-RE)/20.0 ) - 1.E-20
        ampO = 10.0**( (ampO-RE)/20.0 ) - 1.E-20
        sumT = sumT + ampT*ampT
        sumL = sumL + ampL*ampL
        sumO = sumO + ampO*ampO
       end if
      end do
C** Now convert sum to spl as in WOPWOP
      splT = 10.*ALOG10( sumT + 1.E-20) + RE
      splL = 10.*ALOG10( sumL + 1.E-20) + RE
      splO = 10.*ALOG10( sumO + 1.E-20) + RE
cdaw 1-29-2013: added on line to avoid negative values
      if (splT.lt.0.0) splT = 0.0
      if (splL.lt.0.0) splL = 0.0
      if (splO.lt.0.0) splO = 0.0
C** print out the value 
      WRITE(nout,1035) nobs,X,Y,Z,splT,splL,splO
      WRITE(ntek,1040) X,Y,Z,splT,splL,splO
C** Ouput the peak SPL location and magnitude
      if (y.ge.0.0 .and. splO.gt.advmax) then
       advmax = splO
       advx = x
       advy = y
       advz = z
      end if
      if (y.lt.0.0 .and. splO.gt.retmax) then
       retmax = splO
       retx = x
       rety = y
       retz = z
      end if

1035  format(I9,3X,6G12.4)
1040  FORMAT(6G12.4)
      go to 5

100   write(*,1050) nobs
1050  format(I4,' observer locations have been processed.')

      write(*,1059)
      write(*,1060) advmax,advx,advy
      write(*,1061) retmax,retx,rety
      write(nmax,1059)
      write(nmax,1060) advmax,advx,advy
      write(nmax,1061) retmax,retx,rety
1059  format(47X,'x/R     y/R')
1060  format(' Peak SPL  advancing side is ',F5.1,'dB at ',3F8.3)
1061  format(' Peak SPL retreating side is ',F5.1,'dB at ',3F8.3)

      stop
      end
      FUNCTION LENSTR(S)
C
C Returns the number of characters in string S with trailing blanks removed
C
      CHARACTER*(*) S
      DO 10 I=LEN(S),1,-1
        IF(S(I:I).NE.' ')THEN
          LENSTR=I
          RETURN
        ENDIF
 10   CONTINUE
      LENSTR=0
      RETURN
      END

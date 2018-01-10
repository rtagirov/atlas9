      program atlas9

      use string_operations
      use file_operations
      use nlte

      implicit none
 
*
*.... 1994 JAN - MOVED FUNDAMENTAL CONSTANTS INTO COMMON.CONSTB
*.... 1995 JUL - CHANGED freqlg TO freqln = log(freq), IE., A NATURAL LOG
*              - MADE freqlg = log10(freq), IE., A COMMON LOG
*              - ADDED freqln TO common.freqbl
*              - MADE THE CORRESPONDING CHANGES IN kapp
*              - BROUGHT UP TO DATE WITH BOB'S atlas9v.for;83 of 28 JUNE 1995
*.... 1995 OCT - REVERSED THE ORDER OF TESTING j .le. nrhox AND
*                residc(j) .ge. 0.998d0 TO TEST j FIRST TO AVOID ARRARY
*                OUT OF BOUNDS
*
*     2013 hack to streamline for multiple muram model atmospheres 
*		- move reading of model atmospheres into sep routine 
*                 and using sep file (unit=33) might want to change 
*                 and allocate flexible units so that I can run in parallel
*               - flexible units would also apply to current 16 (output) 17??
*               - assume that only one iteration (or none, really) is done, 
*                 but calculate several models (nmod) sequentially
*
*.... THE FOLLOWING FILES ARE USED:
*        FILE 11 = ROSSELAND OPACITY TABLES TO BE READ IF ABUNDANCE NOT 1X SOLAR
*                  THIS FILE IS USED IN SUBROUTINE READIN
*        FILE 12 = MOLECULAR EQUILIBRIUM CONSTANTS
*                  THIS FILE IS USED IN SUBROUTINE READMOL
*        FILE 13 = A FILE CONTAINING A PUNCHED MODEL.  TO BE READ UP TO
*                  BUT NOT INCLUDING THE "BEGIN".  THIS FILE IS USED IN
*                  THE SUBROUTINE READIN
*        FILE 15 = INPUT (for general variables)
*        FILE 16 = OUTPUT
*        FILE 17 = MODEL AND OR FLUX OUTPUT
*        FILE 18 = OUTPUT OF <JNU> FOR USE IN NON-LTE LINE TRANSFER
*        FILE 19 = LINE DISTRIBUTION FUNCTION INPUT WITH A SINGLE VALUE
*                  OF THE MICROTURBULENCE VELOCITY.  IF MICROTURBULENCE
*                  IS TO BE ALLOWED TO VARY WITH DEPTH, THEN A SEPARATE
*                  ODF IS NEEDED FOR EACH VELOCITY.  THIS LEADS TO 
*        FILE 20 = ODF AT 0 KM/S MICROTURBULENCE - BDFXXX[BIG/LIT]0
*        FILE 21 = ODF AT 1 KM/S MICROTURBULENCE - BDFXXX[BIG/LIT]1
*        FILE 22 = ODF AT 2 KM/S MICROTURBULENCE - BDFXXX[BIG/LIT]2
*        FILE 24 = ODF AT 4 KM/S MICROTURBULENCE - BDFXXX[BIG/LIT]4
*        FILE 28 = ODF AT 8 KM/S MICROTURBULENCE - BDFXXX[BIG/LIT]8
*        FILE 33 = actual atm model file with several models in one `row'
*
*----------------------------- COMMONS ---------------------------------
*


      include 'common.odfnlt'

     



      include 'common.sizebl'
      include 'common.constb'
*
      include 'common.abross'
      include 'common.abtotb'
      include 'common.contbl'
      include 'common.convbl'
      include 'common.depart'
      include 'common.edenbl'
      include 'common.elembl'
      include 'common.fluxbl'
      include 'common.freqbl'
      include 'common.freset'
      include 'common.height'
      include 'common.ifblkk'
      include 'common.ifopbl'
      include 'common.ionsbl'
      include 'common.iterbl'
      include 'common.junkbl'
      include 'common.musblk'
      include 'common.opsblk'
      include 'common.optotb'
      include 'common.ptotal'
      include 'common.putblk'
      include 'common.pzerob'
      include 'common.radblk'
      include 'common.rhoxbl'
      include 'common.stateb'
      include 'common.steplg'
      include 'common.taushj'
      include 'common.tcorrb'
      include 'common.teffbl'
      include 'common.tempbl'
      include 'common.turbpr'
      include 'common.waveyb'
      include 'common.xabund'
      include 'common.xnfblk'
      include 'common.xnfpbl'
      include 'common.xnmolb'

      include 'common.freebl'




*
*--------------------------- LOCAL VARIABLES --------------------------
*
      double precision  contin, freq15, part(maxd, 6), rco,  
     &                  rcowt, stepwt, sumwt, wave, x, rosstab, asixth
*     double precision exp10
      integer  i, j, mm,  mode1, n, nsteps, nu, nmod, imod
      logical  finish, more, readi0, stopfl

      real*8, dimension(:), allocatable :: val
*
*-------------------------------  EXTERNALS ----------------------------
*
      external convec, high, hlinop, josh, kapp, linopb, linopl, linopm,
     &         linopv, output, outpt2, outpt3, outpt4, outpt5,
     &         pops, radiap, radiap2, radiap3, readi0, ross, ross2, 
     &         ross3, stateq, stateq2, stateq3, tcorr, tcorr2, tcorr3, 
     &         turb, xlinop, xlisop, rosstab
      character lit*1


*
*--------------------------- INITIALIZATION ----------------------------
*
      data itemp, mode1 / 0, 2 /
*
*------------------------------- EXECUTION -----------------------------
*
*.... INPUT SECTION
*     PREFIX P  PRESSURE
*     PREFIX T  TEMPERATURE
*     PREFIX X  ABUNDANCE FRACTION
*     PREFIX F  IONIZATION FRACTION
*     PREFIX R  FREQUENCY INTEGRAL OR INTEGRATION COEFFICIENT
*     PREFIX A OR AB  MASS ABSORPTION COEFFICIENT
*     PREFIX XNFP  NUMBER DENSITY OVER PARTITION FUNCTION
*
*     ABUND     = THE NORMALLY ASSUMED ABUNDANCES
*     ALPHA     = THE FRACTION OF OPACITY CAUSED BY SCATTERING
*     BHYD      = STATISTICAL EQUILIBRIUM FACTOR FOR HYDROGEN
*     BMIN      = STATISTICAL EQUILIBRIUM FACTOR FOR HMINUS
*     ELEM      = THE LETTER CODES FOR ELEMENTS
*     FREQID    = A LABEL FOR THE FREQUENCY SET
*     IFCORR    = TEMPERATURE CORRECTION .TRUE. = ON OR .FALSE. = OFF
*     IFEMPIR   = FALSE = DEFAULT - A THEORETICAL MODEL
*               = TRUE TO USE AN EMPIRCAL MODEL
*     IFMOL     = TRUE - SET UP EQUILIBRIUM EQUATIONS FOR NUMBER DENSITIES
*               = FALSE -  ASSUME NO MOLECULES AND ITERATE FOR NUMBER DENSITIES
*     IFPRES    = PRESSURE INTEGRATION .TRUE. = ON OR .FALSE. = OFF
*     IFPRNT    = 0 DO NOT PRINT ANYTHING 
*               = 1 PRINT MINIMAL SUMMARY TABLE AND <JUN(J)>
*               = 2 PRINT ALL FREQUENCY INDEPENDENT DATA
*               = 3 PRINT SNU, TAUNU, JNU, ETC.
*               = 4 PRINT OPACITIES
*               = 5 PRINT <JNU>
*     IFPNCH    = 0 DO NOT PUNCH  (no longer makes sense)
*               = 1 PUNCH STRUCTURE 
*               = 2 PUNCH STRUCTURE AND SURFACE FLUX OR INTENSITY
*               = 5 PUNCH 2 AND MOLECULAR NUMBER DENSITIES/PART FNS
*     IFSURF    = 0  CALCULATE FLUX FOR EVERY DEPTH.  THIS IS THE DEFAULT.
*               = 1  CALCULATE FLUX AT SURFACE ONLY
*               = 2  CALCULATE INTENSITY AT SURFACE, SUPPLY NMU ANGLES
*     IFWAVE    = TRUE- STEP NUMNU WAVELENGTHS STARTING AT WBEGIN BY WSTEP
*     LODF      = THE SWITCH THAT TELLS HOW THE LINE OPACITY DISTRIBUTION 
*                 FUNCTION WILL BE USED.  THE OPTIONS ARE: 1) "CONSTANT"
*                 FOR A CONSTANT VTURB WITH DEPTH, 2) "MEMORY  " FOR A 
*                 CONSTANT VTURB ALL IN MEMORY AT ONE TIME, AND 3) "VARIABLE"
*                 FOR A VARIABLE VTURB WITH DEPTH.  THE DEFAULT IS "CONSTANT".
*     NLTEON    = .FALSE. FOR LTE
*               = .TRUE. FOR NLTE
*     NUHI      = NUMBER OF THE FREQUENCY AT WHICH INTEGRATION STOPS
*     NULO      = NUMBER OF THE FREQUENCY AT WHICH INTEGRATION STARTS
*     NUMITS    = NUMBER OF ITERATIONS
*     NUMNU     = NUMBER OF FREQUENCIES IN THE FREQUENCY SET
*     RCOSET    = INTEGRATION COEFFICIENTS FOR THE FREQUENCIES IN FRESET
*     XABUND    = THE ABUNDANCES USED IN THE MODEL
*     XSCALE    = A SCALING FACTOR FOR METAL ABUNDANCES
*     NMOD      = number of models to be calculated
*
*--------------------------- STATEMENT FUNCTION -----------------------
*
c     exp10(x) = exp(x * 2.30258509299405d0)
*

#ifdef ODF
      integer max1, in, nt, nelem6, place   

!   implicit params
      integer  last1, nnnnu, nelem, ion, nu3, iT, iP 
      double precision save, edge,  eq, cutoff, frqlg, contmin 
      double precision freeff
      parameter (max1=maxmol+1)
      
      logical checkinf, isinf, damien 

      double precision xnfh2(maxd), xnfph2(maxd), xnfpco(maxd)
      real*8 XNFP(maxd,10,99),XNFPEL(6,99),DOPPLE(6,99)
      double precision ABLOG(maxd)
!      double precision  wledge(377), cmedge(377),  freqset(1131) 
      double precision a(377), continall(1131, maxd), frqedg(377) 
      double precision CONTABS(1131,maxd)
      double precision CONTSCAT(1131,maxd)
      double precision t4(25),tkev4(25), tk4(25),hkt4(25),tlog4(25)
      double precision  hckt4(25) 
      
      double precision p4(25), xne4(25), xnatom4(25),rho4(25),rhox4(25)
      double precision VTURB4(25),XNFH4(25),XNFHE4(25,2),XNFH24(25)
      real*8 XNFPEL4(6,99,maxd),DOPPLE4(6,99, maxd),CONTINALL4(1131)
      double precision XNFDOPMAX(377,6,99),XNFDOPCONT
!---- for netcdf files ----------------------------------------------!
       integer   ncid, ier, myrank, ntemp, nproc, nufreq, comm 
       character(80) :: name
!---------------------------------------------------------------------
!      common /xnmol/codemol(maxmol), xnfpmol(maxd,maxmol),nummol
       character card*80  
!      dimension card(81)


      DOUBLE PRECISION  WAVEBIG(329)
      DOUBLE PRECISION  BIGA(95),BIGB(125),BIGC(993)
      EQUIVALENCE (WAVEBIG(1),BIGA(1)),(WAVEBIG(96),BIGB(1))
      EQUIVALENCE (WAVEBIG(221),BIGC(1))

      DATA BIGA/
     1     8.97666,     9.2,         9.5,         9.71730,     9.81590,
     2    10.10901,    10.3,        10.46451,    10.65,       10.88545,
     3    11.2,        11.6,        11.95562,    12.3,        12.66565,
     4    12.74676,    12.93504,    13.16033,    13.31723,    13.46700,
     5    13.86253,    14.10462,    14.54782,    14.9,        15.3,
     6    15.74874,    16.00346,    16.4,        16.8,        17.25088,
     7    17.43258,    17.93660,    18.10114,    18.24438,    18.97796,
     8    19.22422,    19.55098,    20.11987,    20.23592,    20.8,
     9    21.3,        21.94783,    22.01983,    22.58070,    22.78377,
     A    23.2,        23.65221,    24.3,        25.00744,    25.46614,
     1    25.89239,    26.14316,    26.6,        27.1,        27.6,
     2    28.09263,    28.65871,    29.3,        29.95849,    30.26260,
     3    31.3,        32.3,        33.3,        34.3,        35.27931,
     4    36.14100,    37.,         38.01600,    38.96642,    40.,
     5    41.,         41.87992,    42.5,        43.5,        44.73260,
     6    45.5,        46.5,        47.5,        48.50178,    49.5,
     7    50.42590,    50.87630,    52.,         54.,         56.,
     8    57.40614,    59.5,        61.5,        63.5,        65.53786,
     9    67.,         69.,         71.,         72.24011,    74./
      DATA BIGB/
     1    76.,         78.,         80.,         82.,         84.,
     2    86.,         88.,         90.,         91.17535,    94.,
     3    98.,         102.,       106.,        110.03056,   113.,
     4   116.,         120.,       123.92928,   128.,        132.,
     5   136.,         140.,       144.43391,   148.,        151.43485,
     6   157.,         162.15072,  167.40282,   171.,        175.,
     7   180.,         184.,       188.,        193.,        197.46990,
     8   202.,         207.13210,  210.,        215.,        220.,
     9   225.,         230.,       235.,        240.,        245.,
     A   251.12621,    255.,       260.,        265.,        270.,
     1   275.,         280.,       285.,        290.,        300.,
     2   310.,         320.,       330.,        340.,        350.,
     3   360.,         364.70183,  370.,        380.,        390.,
     4   400.,   410., 420., 430., 450., 460.,  470., 480.,  490., 500.,
     5   510.,   520., 530., 540., 550., 560.,  570., 580.,  590., 600.,
     6   610.,   620., 630., 640., 650., 660.,  670., 680.,  690., 700.,
     7   710.,   720., 730., 740., 750., 760.,  770., 780.,  790., 800.,
     8   810.,820.58271,830.,840., 850., 860.,  870., 880.,  890., 900.,
     9   910.,   920., 930., 940., 950., 960.,  970., 980.,  990.,1000./
      DATA BIGC/
     1 1025.,1050.,1075.,1100.,1125.,1150.,1175.,1200.,1225.,1250.,
     2 1275.,1300.,1325.,1350.,1375.,1400.,1425.,1458.81670,1475.,1500.,
     3 1525.,1550.,1575.,1600.,1640.,1680.,1720.,1760.,1800.,1840.,
     4 1880.,1920.,1960.,2000.,2050.,2100.,2150.,2200.,2250.,2279.40330,
     5 2300.,2350.,2400.,2450.,2500.,2550.,2600.,2650.,2700.,2750.,
     6 2800.,2850.,2900.,2950.,3000.,3050.,3100.,3150.,3200.,3282.34320,
     7 3400.,3500.,3600.,3700.,3800.,3900.,4000.,4100.,4200.,4300.,
     8 4400.,4500.,4600.,4700.,4800.,4900.,5000.,5100.,5200.,5300.,
     9 5400.,5500.,5600.,5700.,5800.,5900.,6000.,6100.,6200.,6300.,
     A 6400.,6600.,6800.,7000.,7200.,7400.,7600.,7800.,8000.,8200.,
     1 8400.,8600.,8800.,9000.,9200.,9400.,9600.,9800.,10000000.,
     2 884*0./



      myrank = 0
      nufreq = 1131
 
      damien = .true.
      data idmol / 101.,  106.,  107.,  108.,  606.,  607.,  608.,
     & 707.,  708.,
     &   808.,  112.,  113.,  114.,  812.,  813.,  814.,  116.,  120.,
     &   816.,  820.,  821.,  822.,  823.,  103.,  104.,  105.,  109.,
     &   115.,  117.,  121.,  122.,  123.,  124.,  125.,  126.,106.01,
     & 107.01,108.01,112.01,113.01,114.01,120.01,  408.,  508.,  815.,
     &   817.,  824.,  825.,  826.,10108.,60808.,10106.,60606.,  127.,
     &   128.,  129.,  827.,  828.,  829., 608.01 / 
 
      data momass / 2.,   13.,   15.,   17.,   24.,   26.,   28.,   28.,
     &   30.,
     &    32.,   25.,   28.,   29.,   40.,   43.,   44.,   33.,   41.,
     &    48.,   56.,   61.,   64.,   67.,    8.,   10.,   12.,   20.,
     &    32.,   36.,   46.,   49.,   52.,   53.,   56.,   57.,   13.,
     &    15.,   17.,   25.,   28.,   29.,   41.,   25.,   27.,   47.,
     &    51.,   68.,   71.,   72.,   18.,   44.,   14.,   36.,   60.,
     &    59.,   64.,   75.,   74.,   79.,   28. /







! READ in input header first, as later on you want to be able to allocate 
! the right size array for small and big wl bins (not subbins)!

! ......


*
*.... OPEN THE I/O FILES
*
      open (unit = 15, file = 'atlas9.input', form = 'formatted',
     &      status = 'old', access = 'sequential')
      open (unit = 16, file = 'atlas9.print', form = 'formatted',
     &      status = 'new', access = 'sequential')
      open (unit = 17, file = 'atlas9.punch', form = 'formatted',
     &      status = 'new', access = 'sequential')
      open (unit = 18, file = 'atlas9.jnu', form = 'formatted',
     &      status = 'new', access = 'sequential')
      open (unit = 33, file = 'atlas9.modinp', form = 'formatted',
     &      status = 'old', access = 'sequential')

!Output files:
!      open (unit = 10, file ='xnfpdf.dat', form = 'unformatted',
!     &      status = 'new')
!
!      open (unit = 11, file ='xnfpdfmax.dat', form = 'unformatted',
!     &      status = 'new')
!---------------------------------------------------------------------
     
      open (unit = 77, file ='debug.dat', form = 'formatted',
     &      status = 'new')

!   READ in continua.dat 

      open (unit = 44, file = 'continua.dat', form = 'formatted',
     &      status = 'old', access = 'sequential') 

      more = .true.
!
      maxpow = 99
      last=80
      numcol=1
      in = 0
!----------------------------------------------------------------

      if (damien ) then 


      do i=1, 329

       wledge(i) = wavebig(i)
       cmedge(i) = 1.0d7/wavebig(i) 
       frqedg(i) = c_nm/wavebig(i) 

      end do

      in = 329


      else 

      place = 0  
      i = 0 
      do while (more)
! read file 44: continua.dat the final line has to have 'begin'! 
        i = i +1
        numcol=1
        if (place .eq. 0) then 
         read(44,100) card
100      format(a)
         print*, card
        end if
        if (i .gt. 1000) then 
          more = .false.
        end if
       if (card .eq. 'begin' ) then 
        more = .false.
       else
        edge = freeff(card, place) 
        if (edge .eq. 0 )  then 
         place = 0  
        else 

        in= in+1
         print *, in, edge
  
         if (abs(edge) .lt. 1.0d6) then
            wledge(in) = edge
            cmedge(in) = 1.0d7/edge
            frqedg(in) = c_nm/wledge(in)

         else if (abs(edge) .lt. 1.0d25) then 
            frqedg(in) = edge
            wledge(in) = c_nm/edge
            cmedge(in) = 1.0d7/wledge(in) 


         else
            cmedge(in) = edge/1.0d25
            wledge(in) = 1.0d7/cmedge(in)
            frqedg(in) = c_nm/wledge(in)

         end if
         a(in) = abs(wledge(in))
 
         write(6,15) in,frqedg(in), wledge(in), cmedge(in)
15       format(i5, 1pe25.15, 0pf20.7, f20.7)
        end if 
       end if
  
      end do
 
      
!  sort a, in ascending order
! (think if you can do that more efficiently...) 
!
      do last= 2, in
       last1= in -last +2

         do i= 2, last1
          if (a(i) .lt. a(i-1)) then
          
            save  = a(i-1)
            a(i-1)= a(i) 
            a(i)  = save 

            save  = frqedg(i-1)
            frqedg(i-1) = frqedg(i)
            frqedg(i)   = save

            save        = wledge(i-1)
            wledge(i-1) = wledge(i)
            wledge(i)   = save
           
            save        = cmedge(i-1)
            cmedge(i-1) = cmedge(i)
            cmedge(i)   = save
 
          end if
         end do

      end do


      endif
!----------------------------------------------------------------------


      write(6,15)(i, frqedg(i), wledge(i), cmedge(i), i=1, in)

      numnu=0
      do i=1, in-1
       numnu= numnu +1
       freqset( numnu ) = abs(frqedg(i))/1.0000001
       numnu= numnu +1
       freqset( numnu ) = c_nm/(abs(wledge(i)) + abs(wledge(i+1)))*2.0d0
       numnu = numnu +1
       freqset( numnu ) = abs(frqedg(i+1))*1.0000001
 
      end do 
    
      write(6,15) numnu
      nnnnu= numnu

!---------- only now read the model
      more = .true.
!-------------------------------------------------------------------
! call reading model (reads header and how many atmosphere models
      more = readi0 (mode1)
         if(more) then
            read (33,*) nmod ! this will be the amount of T values 
            read (33,*) nrhox ! this will be the amount of p values 

            if (nrhox .gt. nmod ) then 
               write( *, *) 'amount of p values is > than # of T values'
               stop
            end if  
 
            if(nmod .gt. maxd) then
               write( *,*) ' nrhox = ', nmod, ', which is > maxd'
               stop
            end if
         end if

!     READ MODEL (first nmod T, then nrhox p values)
!     Because we need one atmosphere at a time keep T fixed
!     and calculate for all p values -> for not pretabulated 
!     calculations we need to adjust here!!! 
!-------------------------------------------------------!
            
               read (33, *) (tsave(j), j = 1, nmod)
               read (33, *) (p(j), j=1, nrhox)
!-------------------------------------------------------!
! now we now dimensions of the calculations and can creat 
!  NetCDF file                                          !
      ntemp = nmod
      nproc = 1
      comm = 1
      call CreateContinum( myrank, ncid, 'Continum.nc', nrhox, ntemp, 
     &                   nufreq, ier )
      name = 'Continum.nc'


      numnu   = nnnnu
      cutoff  = 1.0d-03
      ifop(14)= 0
      ifop(15)= 0
      ifop(16)= 0
      ifop(17)= 0
      ifpres = 1
      iter   = 1 
      numits = 1
      glog = 0.0d0
! start loop over different p and T
! use a certain tsave paired with all nrhox values of p

     
      do i=1, nmod



       nproc =i


       do j= 1, nrhox
         t(j) = tsave(i)
         tk(j)     = k * t(j)
         hckt(j)   = hc / tk(j)
         hkt(j)    = h / tk(j)
         tkev(j)   = k_ev * t(j)
         tlog(j)   = log(t(j))
         vturb(j)  = 0.0d0
         xnatom(j) = p(j) / tk(j) - xne(j)
         rho(j)    = xnatom(j) * wtmole * 1.660d-24
          if (ifturb) pturb(j) = 0.5 * rho(j) * vturb(j) ** 2
 
       end do 

         nt = nrhox
         teff = t(1)

!       write(10)nt,teff,glog,title
       write(11)nt,teff,glog,title
!       write(10)in,(frqedg(j),wledge(j),cmedge(j),j=1,in),idmol,momass
       write(11)in,(frqedg(j),wledge(j),cmedge(j),j=1,in)
!       write(10)numnu,(freqset(nu),nu=1,numnu)
       write(11)numnu,(freqset(nu),nu=1,numnu)
 

       itemp = itemp + 1
 
! --- Calculate Saha for certain elements ----!
! carefull xnfh was a xnfh(nrohx) array but in the
! new atlas it is xnfh(nrhox,2) --> what is the 2nd index --> it is the first and second
! and nth ion!!!

        call pops(1.00d0, 12, xnfh)
        call pops(2.01d0, 12, xnfhe)

        call pops( 1.01d0, 11, xnfph)
        call pops( 2.02d0, 11, xnfphe)
        call pops( 5.00d0, 11, xnfpb)
        call pops( 6.01d0, 11, xnfpc)
        call pops( 8.00d0, 11, xnfpo)
        call pops(11.00d0, 11, xnfpna)
        call pops(12.01d0, 11, xnfpmg)
        call pops(13.01d0, 11, xnfpal)
        call pops(14.01d0, 11, xnfpsi)
        call pops(19.00d0, 11, xnfpk)
        call pops(20.01d0, 11, xnfpca)
        call pops(26.00d0, 11, xnfpfe)

        IF (IFMOL .EQ. 1) CALL POPS(106.00D0, 11, XNFPCH)
        IF (IFMOL .EQ. 1) CALL POPS(108.00D0, 11, XNFPOH)

        do j = 1, nrhox

         xnfh2(j) = 0.0d0

         if (t(j) .le. 9000) then
          eq=exp(4.478/tkev(j)-4.64584d1+(1.63660d-3+(-4.93992d-7
     &       +(1.11822d-10+(-1.49567d-14+(1.06206d-18-
     &        3.08720D-23*t(j))*t(j))*t(j))*t(j))*t(j))*t(j)
     &       -1.5*tlog(j))

           xnfh2(j) = xnfh(j,1)**2*eq
           xnfph2(j) = xnfph(j,1)**2*eq 
           xnfpco(j) = xnfpc(j,1)*xnfpo(j,1)*exp(11.091/tkev(j)-49.0414+
     &            14.0306d-4*t(j)-26.6341d-8*t(j)**2+35.382d-12*t(j)**3-
     &            26.5424d-16*t(j)**4+8.32385d-20*t(j)**5-1.5*tlog(j))
         end if 
       
        end do 


        do nu=1, numnu
         freq=freqset(nu)
         freq15 = freq/1.0d15
         rco = 0.0d0
         frqlg = log(freq)
         freqln = frqlg
  
         do j = 1, nrhox
           ehvkt(j) = exp(-freq*hkt(j))
           stim(j) = 1.0d0 - ehvkt(j)
           bnu(j) =1.47439d-02*freq15**3*ehvkt(j)/stim(j)  
           dbnudt(j) = bnu(j) * freq * hkt(j) / t(j) /
     &                           stim(j)
          if(numnu .eq. 1) dbnudt(j) = 4.0d0 * sigma / pi *
     &                                 t(j) ** 3




         end do 

         n=1
         nsteps = 1
         stepwt = 1.0d0 

         wave =c_nm/freq 
         waveno= 1.0d7/wave 
         call kapp ! (n, nsteps, stepwt) 

         WRITE(6,15) nu, freq, wave, waveno 
            
         do j = 1, nrhox
           abtot(j) = (acont(j)+sigmac(j))
           continall(nu,j) =log10(abtot(j))
           contabs(nu,j) = (log10((acont(j))))
           contscat(nu, j) =log10( sigmac(j))
           ablog(j) = log10(abtot(j))
  
         end do
!105       format(f5.2) 
         write(6,*)(ablog(j), j=1, nrhox)

        end do !end of nu loop

        do j=1,nrhox
         t4(j)=t(j)
         tkev4(j)=tkev(j)
         tk4(j)=tk(j)
         hkt4(j)=hkt(j)
         tlog4(j)=tlog(j)
         hckt4(j)=hckt(j)
         p4(j)=p(j)
         xne4(j)=xne(j)
         xnatom4(j)=xnatom(j)
         rho4(j)=rho(j)
         rhox4(j)=rhox(j)
         vturb4(j)=vturb(j)
!   careful xnfh had only one dimension in xnfdf
!   now the second script I set to 1, but I have to check!
 
         xnfh4(j)=xnfh(j,1)
         xnfhe4(j,1)=xnfhe(j,1)
         xnfhe4(j,2)=xnfhe(j,2)
         xnfh24(j)=xnfh2(j)

        end do

!             
!        write(10)t4,tkev4,tk4,hkt4,tlog4,hckt4,p4,xne4,xnatom4,rho4,
!     &    rhox4,vturb4,xnfh4,xnfhe4,xnfh24

        do nelem = 1, 99
          do ion = 1, 10
            do  j=1, nrhox
              xnfp(j,ion,nelem)=0.0d0
            end do
          end do
        end do 


        call pops(1.01d0,11,xnfp(1,1,1))
        call pops(2.02d0,11,xnfp(1,1,2))
        call pops(3.03d0,11,xnfp(1,1,3))
        call pops(4.03d0,11,xnfp(1,1,4))
        call pops(5.03d0,11,xnfp(1,1,5))
        call pops(6.05d0,11,xnfp(1,1,6))
        call pops(7.05d0,11,xnfp(1,1,7))
        call pops(8.05d0,11,xnfp(1,1,8))
        call pops(9.05d0,11,xnfp(1,1,9))
        call pops(10.05d0,11,xnfp(1,1,10))
        call pops(11.05d0,11,xnfp(1,1,11))
        call pops(12.05d0,11,xnfp(1,1,12))
        call pops(13.05d0,11,xnfp(1,1,13))
        call pops(14.05d0,11,xnfp(1,1,14))
        call pops(15.05d0,11,xnfp(1,1,15))
        call pops(16.05d0,11,xnfp(1,1,16))
        call pops(17.04d0,11,xnfp(1,1,17))
        call pops(18.04d0,11,xnfp(1,1,18))
        call pops(19.04d0,11,xnfp(1,1,19))
        call pops(20.09d0,11,xnfp(1,1,20))
        call pops(21.09d0,11,xnfp(1,1,21))
        call pops(22.09d0,11,xnfp(1,1,22))
        call pops(23.09d0,11,xnfp(1,1,23))
        call pops(24.09d0,11,xnfp(1,1,24))
        call pops(25.09d0,11,xnfp(1,1,25))
        call pops(26.09d0,11,xnfp(1,1,26))
        call pops(27.09d0,11,xnfp(1,1,27))
        call pops(28.09d0,11,xnfp(1,1,28))
 

        do  nelem=29,99
          call pops(float(nelem)+.02d0,11,xnfp(1,1,nelem))
        end do

         
        do  j=1,nrhox
          xnfp(j,10,40)=xnfph2(j)
          xnfp(j,10,46)=xnfpco(j)
        end do

      
        if(ifmol .eq. 1 ) then 
          do nelem=40,99
             call pops(idmol(nelem-39),1,xnfp(1,6,nelem))
          end do
        end if

        do  j=1,nrhox
          do  nelem=20,28
            xnfp(j,5,30+nelem)=xnfp(j,7,nelem)
            xnfp(j,5,40+nelem)=xnfp(j,8,nelem)
            xnfp(j,5,50+nelem)=xnfp(j,9,nelem)
            xnfp(j,5,60+nelem)=xnfp(j,10,nelem)
          end do
        end do



        do  nelem=1,99
          do  ion=1,6
           nu3=0
           do  nu=1,1131,3
            nu3=nu3+1
            xnfdopmax(nu3,ion,nelem)=0.
           end do
          end do
        end do 


!-----------------------------------------------      
!     large nrhox 300 loop
!----------------------------------------------
        do  j=1,nrhox

           do  nelem=1,99
             do  ion=1,6
               xnfpel(ion,nelem)=xnfp(j,ion,nelem)
             end do
           end do




           do  nelem=1,99
             dopple(1,nelem)=sqrt(2.*tk(j)/atmass(nelem)/1.660d-24+
     &                      vturb(j)**2)/2.99792458d10
             dopple(2,nelem)=dopple(1,nelem)
             dopple(3,nelem)=dopple(1,nelem)
             dopple(4,nelem)=dopple(1,nelem)
             dopple(5,nelem)=dopple(1,nelem)
             dopple(6,nelem)=dopple(1,nelem)
           end do


           do  nelem=20,28
             dopple(5,30+nelem)=dopple(1,nelem)
             dopple(5,40+nelem)=dopple(1,nelem)
             dopple(5,50+nelem)=dopple(1,nelem)
             dopple(5,60+nelem)=dopple(1,nelem)
           end do
!if was commented out, but it does not make sense... check!!!
!------------------------------------------------------------------
           if(ifmol .eq. 1 ) then
             do  nelem=40,99
               dopple(6,nelem)=sqrt(2.*tk(j)/momass(nelem-39)/1.660d-24)
     &                         /2.99792458d10
             end do 
           end if


           do  nelem=1,99
            do  ion=1,6
              xnfpel4(ion,nelem, j)=xnfpel(ion,nelem)
              dopple4(ion,nelem,j)=dopple(ion,nelem)

            end do
           end do 

           nu3=0
           do  nu=1,numnu,3
              nu3=nu3+1

           if (j .eq. 15) then
!           write(77,*) 'xnfpel(1,1), dopple(1,1), freqset(nu), nu'
!           write(77,*)  xnfpel(1,1), dopple(1,1), freqset(nu), nu
!           write(77,*) 'rho(15),  cutoff', rho(15), cutoff 
           end if

              contmin=min(continall(nu,j),continall(nu+1,j),
     &                 continall(nu+2,j))
              contmin=10.**contmin
              do nelem=1,99
                do  ion=1,6
                   xnfdopcont=xnfpel(ion,nelem)/dopple(ion,nelem)/
     &                         freqset(nu+1)/rho(j)/contmin/cutoff
              xnfdopmax(nu3,ion,nelem)=max(xnfdopmax(nu3,ion,nelem)
     &                                       ,xnfdopcont)
             end do
            end do
          if (j .eq. 15) then
!          write(77,*) 'contmin', contmin
!          write(77,*) 'xnfdopmax (nu3,1,1)', xnfdopmax(nu3,1,1)        
          endif

           end do



          it=itemp
          ip=j
!--------------------------------------------------------------------

           do  nu=1,1131
              continall4(nu)=continall(nu,j)
           end do
!           damien = .false. 
           if(damien ) then 
           write(77,*) 'Header: freqset (984)'
           write(77,*) (freqset(nu), nu=1, 984)
           write(77,*) 'Header: wavelength gird in nm '
           write(77,*) (c_nm/freqset(nu), nu=1,984) 
           write(77,*) 'Header: T grid, with 57 values'
           write(77,*) (tsave(nu), nu=1, 57)
           write(77,*) 'Header: P grid, with 25 values'
           write(77,*) (p(nu), nu=1, 25) 
           damien = .false.
           write(77,*) ' from now on: first p,T values followed by '
           write(77,*) ' total continum for all freqset, then scat op'


           write(77,*) p(j)
           write(77,*) t(1)
           write(77,*) (continall4(nu), nu=1, 984) 
           write(77,*) (contscat(nu,j), nu=1, 984)
 
           endif           


!           WRITE(10,*)'XNFPEL4'
!           WRITE(10,*)XNFPEL4
!           WRITE(10,*)'DOPPLE4'
!           WRITE(10,*)DOPPLE4
!           WRITE(10,*)'CONTINALL4'
!           WRITE(10,*)CONTINALL4

!           write(10)xnfpel4,dopple4,continall4
!           write(6,280)j
!280         format(18h0xnfpel.....dopple,i10)
 

!           do  nelem=1,39
!           write(6,290)nelem,(xnfpel(ion,nelem),ion=1,6),dopple(1,nelem)
!290         format(i5,1p6e12.4,5x,2e12.4,0pf10.2,i5)
!           end do


!           do  nelem=40,99
!            nelem6=nelem*6
!            write(6,290)nelem,(xnfpel(ion,nelem),ion=1,6),
!     &        dopple(1,nelem),dopple(6,nelem),idmol(nelem-39),nelem6
!           end do        
!            write(6,333) ' T ', t(j),' TK ', tk(j), 'HKT ', hkt(j),
!     &           'TKEV  ', tkev(j),' TLOG ', tlog(j), 'RHOX  ', rhox(j),
!     &           ' P ', p(j),' XNE ',xne(j), ' RHO ' , rho(j), 
!     &      ' xnfh ', xnfh(j,1),' xnfhe ', xnfhe(j,1),' xnfh2 ',xnfh2(j)

!333       format(a ,1pe12.4, a  ,e12.4,a   ,
!     &  e12.4,a  ,e12.4,a    ,e12.4/a   ,
!     &  e12.4,a  ,e12.4,a  ,e12.4,a   ,e12.4/a,
!     &  e12.4,a  ,e12.4,a  ,e12.4)




        end do
!----- end of large nrhox 300 loop
!-------------------------------------------------------------------------
       call WriteContinum( ncid, myrank, nproc, comm,
     &      wledge, freqedge, cmedge, idmol, momass, freqset,
     &      tsave, p, t4, rho4, xne, xnatom, vturb, xnfh24, xnfhe4,
     &      xnfh4, continall, contscat, xnfpel4, xnfdopmax, dopple4,
     &      nrhox, ntemp, nufreq , ier)



!      call OpenContinum(  ncid, name, nrhox, ntemp, nnnnu, ier)


         write(11)xnfdopmax
!        write(6,'(1h1)')


      end do

!     do i=1,20  
!
!       nproc =i
!             call ReadContinum ( myrank, comm, nproc, ncid,
!     &      wledge, freqedge, cmedge, idmol, momass, freqset,
!     &      tsave, p, t4, rho4, xne, xnatom, vturb, xnfh24, xnfhe4,
!     &      xnfh4, continall, contscat, xnfpel4, xnfdopmax, dopple4,
!     &      nrhox, ntemp, nnnnu, ier)
!      end do

      call CloseNetCDF (myrank, ncid, ier)



!---- END of the continuum calculations
!--------------------------------------------------------!

#else

      asixth = 1.26d-15

*
*.... OPEN THE I/O FILES
*
      open (unit = 15, file = 'atlas9.input', form = 'formatted',
     &      status = 'old', access = 'sequential')

      open (unit = 33, file = atm_mod_file, form = 'formatted',
     &      status = 'old', access = 'sequential')

!      open (unit = 16, file = 'atlas9.print', form = 'formatted',
!     &      status = 'new', access = 'sequential')
      open (unit = 16, file = print_file,
     &      status = 'replace', access = 'stream',
     $      form = 'formatted')

!      open (unit = 17, file = 'atlas9.punch', form = 'formatted',
!     &      status = 'new', access = 'sequential')
      open (unit = 17, file = spec_file,
     &      status = 'replace', access = 'stream',
     $      form = 'formatted')

!      open (unit = 18, file = 'atlas9.jnu', form = 'formatted',
!     &      status = 'new', access = 'sequential')
      open (unit = 18, file = 'atlas9.jnu',
     &      status = 'replace', access = 'stream',
     $      form = 'formatted')

!      open (unit = 3, file = 'continuum.dat', form = 'formatted',
!     &      status = 'new', access = 'sequential') !write file in josh and close here at the end! 


*.... open (unit = 41, file = 'atlas9.wave', form = 'formatted',
*....&      status = 'new', access = 'sequential')
*
*.... THE ODF FILES ARE OPENED IN READIN WHEN IT HAS IFOP TO CHECK
*.... THEY ARE ALSO CLOSED IN READIN WHEN IT SEES "END", EXCEPT FOR
*.... THE CASE WHEN THE WHOLE ODF IS IN MEMORY, IN WHICH CASE THE
*.... FILE IS CLOSED IN LINOPM AFTER THE CONTENTS ARE READ IN.
*
*.... PUT THE WHOLE THING IN A LOOP TO AVOID A GO TO AT THE END
      more = .true.
      lit = 'y'
!!! DO LOOP (1)------------------------------------------------
!-------------------------------------------------------------*
      do while (more)
         more = readi0 (mode1)
*
         if(more) then

!            read (33,*) nmod
!            read (33,*) nrhox

            nmod = 1

            nrhox = num_of_lines(atm_mod_file)

            if(nrhox .gt. maxd) then
               write( *,*) ' nrhox = ', nrhox, ', which is > maxd'
               stop
         end if
*
*     WRITE TITLE INFO TO OUTPUT FILES
*
            call output
*
*.... BEGINNING OF THE MODEL LOOP
!---DO Loop (2) ----------------------------------------------*
!
! This loops over several models, if they are provdied
! nmod = number of models ------------------------------------!
            do imod = 1, nmod

!               open (unit = 41, file = 'atlas9.wave',
!     &        form = 'formatted', status = 'new',
!     &        access = 'sequential')

!               open (unit = 41, file = 'atlas9.wave',
!     &               status = 'replace', access = 'stream',
!     $               form = 'formatted')
* 
*     READ MODEL
*
               read (33, *) (rhox(j), t(j), p(j), xne(j),
     &            abross(j), accrad(j), vturb(j), j = 1, nrhox)

!---------- calculate rosseland tau--------------------------------------!
!    call integ (rhox, abross, tauros, (nrhox), (abross(1)*rhox(1)))
!         do j=1, nrhox
!            print*, j , tauros(j)
!         end do 

*
*.... HERE GOES A BODGE TO CHECK WHETHER ABROSS HAS BEEN 
*     PROPERLY CALCULATED  IN THE MODEL ATMOSPHERE
*
               if (abross(nrhox).lt. 1e-08) then
                  do j = 1, nrhox
                     abross(j) = rosstab(t(j), p(j), vturb(j))
                  end do
               end if
*
*.... ADD HERE SOME BOUNDARY PRESSURE INITIALIZATIONS 
*
               pradk0 = asixth * teff**4 
!               pradk0 = 0.0d0
               pturb0 = pturb(1)
               pcon   = 0.0d0
               pzero  = pcon + pturb0+  pradk0 
*
*     SHOULD PROBABLY CHANGE THE FOLLOWING AND ALLOW FOR PRADK0 NOT BEING GIVEN
*         could use the T^4 prescription similar to readin routine 
!
!Prad is set initially to zero and is calculated here:
               
               call integ (rhox, accrad, prad, (nrhox),
     &                     (accrad(1) * rhox(1)))
*
!---------- xne is read in -

               do j = 1, nrhox
                  pradk(j)  = prad(j) + pradk0
               end do

!               print *, 'Here the calculated xnatom and rho'

               do j = 1, nrhox
                  tk(j)     = k * t(j)
                  hckt(j)   = hc / tk(j)
                  hkt(j)    = h / tk(j)
                  tkev(j)   = k_ev * t(j)
                  tlog(j)   = log(t(j))
                  xnatom(j) = p(j) / tk(j) - xne(j)
                  rho(j)    = xnatom(j) * wtmole * 1.660d-24
!                  vwrho(j) = rho(j)  
                 if (ifturb) pturb(j) = 0.5 * rho(j) * vturb(j) ** 2
!------------ print/write for debugging reasons
!               print * , 'v_turb ()', vturb(j) 
!                write (7,*) xnatom(j), rhox(j), rho(j) 

!                 write(*, '(a,2x,i2,2x,es10.3)'), '1st', j, xnatom(j)

               enddo
*
c	       print *, ' *** Model ', imod 
               iter  = imod 
*
*.... CHANGING ITEMP TELLS THE SUBROUTINES THEY HAVE A NEW TEMPERATURE
*
               itemp = itemp + iter
!  only if pressure it on the hydrostatic eq. is solved
!--------------------------------------------------------------!
*
               if(ifpres) then
*
*.... INTEGRATE THE EQUATION OF HYDROSTATIC EQUILIBRIUM
*
                  pzero = pcon + pradk0 + pturb0
*
                  do j = 1, nrhox
                     ptotal(j) = grav * rhox(j) + pzero
                     p(j) = grav * rhox(j) - prad(j)-pturb(j)-pcon
!--------------- print for debugging reasons --------------------------
!                     print *, 'rhox(j) and prad(j)', rhox(j), prad(j)
!                     print *, 'grav, pturb, pcon', grav, pturb(j), pcon
!                     print *, 'p, rhox and prad ', p(j),rhox(j), prad(j)
                   if (p(j) .le. 0.0d0) then
                     p(j)= grav*rhox(j)-(prad(j)-prad(1))-pturb(j)-pcon
!                     print *, 'new pressure ', p(j)
                       if (p(j) .le. 0.0d0) then
                          write (16, 9002)
     &                     ' pressure le 0.0d0 at depth', j,
     &                     ' pzero = ', pzero,
     &                     'j', 'p(j)', 'accrad(j)', 'prad(j)',
     &                     (i, p(i), accrad(i), prad(i), i = 1, j)
9002                    format (a, i5, a, 1pe15.4, / ,
     &                         4x, a, 8x, a, 9x, a, 7x, a /
     &                         (i5, 3e15.4))
!                        print *, ' pressure le 0 at depth ', j
                        stop
                      end if
                     end if
*
                  end do
!-----------------------------------------------------------*
                  ifedns = .false.

                  call pops(0.0d0, 1, xne)
               end if

               call system('rm -vf hydpops.out')
               call system('rm -vf hminus.out')

               call read_nlte_file()

               if (.not. nltepop) then

                    call pops(0.0d0, 1, xne)

!                    do j = 1, nrhox

!                       write(*, '(A,2x,i2,2x,es10.3)'), '2nd', j, xnatom(j)
!                       print*, '2nd', j, xnatom(j)

!                    enddo

               endif

               if (nltepop) then

                  allocate(bhii(nrhox))
                  allocate(val(nrhox))

                  call read_nlte_lev(nrhox, 'ELECTR', 'pop', val)

                  xne(1 : nrhox) = xnatom(1 : nrhox) * val(1 : nrhox)

                  call read_nlte_lev(nrhox, 'HMINUS', 'dep', bmin)

                  do n = 1, 8

                    call read_nlte_lev(nrhox, 'HI'//int_to_char(n), 
     $                                 'dep', val)

                    bhyd(1 : nrhox, n) = val(1 : nrhox)

                  enddo

                  call read_nlte_lev(nrhox, 'HII', 'dep', bhii)

!                  bhii(1 : nrhox) = 1.0

                  deallocate(val)

               endif

               call pops( 1.01d0, 11, xnfph)
               call pops( 2.02d0, 11, xnfphe)
               call pops( 6.03d0, 11, xnfpc)
               call pops( 7.04d0, 11, xnfpn)
               call pops( 8.05d0, 11, xnfpo)
               call pops(10.05d0, 11, xnfpne)
               call pops(12.01d0, 11, xnfpmg)
               call pops(13.00d0, 11, xnfpal)
               call pops(14.01d0, 11, xnfpsi)
               call pops(20.01d0, 11, xnfpca)
               call pops(26.00d0, 11, xnfpfe)

               if (.not. ifmol) then

                  call pops( 1.01d0, 12, xnfh)
                  call pops( 2.02d0, 12, xnfhe)
                  call pops( 6.05d0, 12, xnfc)
                  call pops( 7.05d0, 12, xnfn)
                  call pops( 8.05d0, 12, xnfo)
                  call pops(10.05d0, 12, xnfne)
                  call pops(12.05d0, 12, xnfmg)
                  call pops(14.05d0, 12, xnfsi)
                  call pops(16.05d0, 12, xnfs)
                  call pops(26.04d0, 12, xnffe)

               else

                  call pops(106.00d0, 11, xnfpch)
                  call pops(108.00d0, 11, xnfpoh)
*
*...  THE POPS WILL NOT RETURN NUMBER DENSITIES WHEN MOLECULES ARE ON
*...  SO WE COMPUTE NUMBER DENSITIES/PART FUNCTIONS  AND PART FUNCTIONS
*
                  call pops( 6.05d0, 11, xnfc)
                  call pops( 7.05d0, 11, xnfn)
                  call pops( 8.05d0, 11, xnfo)
                  call pops(10.05d0, 11, xnfne)
                  call pops(12.05d0, 11, xnfmg)
                  call pops(14.05d0, 11, xnfsi)
                  call pops(16.05d0, 11, xnfs)
                  call pops(26.04d0, 11, xnffe)

                  do j = 1,nrhox

                     call pfsaha(j, 1, 1, 3, part)
                     xnfh(j, 1) = xnfph(j, 1) * part(j, 1)
                     xnfh(j, 2) = xnfph(j, 2)
*
                     call pfsaha(j, 2, 2, 13, part)
                     xnfhe(j, 1) = xnfphe(j, 1) * part(j, 1)
                     xnfhe(j, 2) = xnfphe(j, 2) * part(j, 2)
                     xnfhe(j, 3) = xnfphe(j, 3)
*
                     call pfsaha(j, 6, 6, 13, part)
                     xnfc(j, 1) = xnfc(j, 1) * part(j, 1)
                     xnfc(j, 2) = xnfc(j, 2) * part(j, 2)
                     xnfc(j, 3) = xnfc(j, 3) * part(j, 3)
                     xnfc(j, 4) = xnfc(j, 4) * part(j, 4)
                     xnfc(j, 5) = xnfc(j, 5) * part(j, 5)
                     xnfc(j, 6) = xnfc(j, 6) * part(j, 6)
*
                     call pfsaha(j, 7, 6, 13, part)
                     xnfn(j, 1) = xnfn(j, 1) * part(j, 1)
                     xnfn(j, 2) = xnfn(j, 2) * part(j, 2)
                     xnfn(j, 3) = xnfn(j, 3) * part(j, 3)
                     xnfn(j, 4) = xnfn(j, 4) * part(j, 4)
                     xnfn(j, 5) = xnfn(j, 5) * part(j, 5)
                     xnfn(j, 6) = xnfn(j, 6) * part(j, 6)
*
                     call pfsaha(j, 8, 6, 13, part)
                     xnfo(j, 1) = xnfo(j, 1) * part(j, 1)
                     xnfo(j, 2) = xnfo(j, 2) * part(j, 2)
                     xnfo(j, 3) = xnfo(j, 3) * part(j, 3)
                     xnfo(j, 4) = xnfo(j, 4) * part(j, 4)
                     xnfo(j, 5) = xnfo(j, 5) * part(j, 5)
                     xnfo(j, 6) = xnfo(j, 6) * part(j, 6)
*
                     call pfsaha(j, 10, 6, 13, part)
                     xnfne(j, 1) = xnfne(j, 1) * part(j, 1)
                     xnfne(j, 2) = xnfne(j, 2) * part(j, 2)
                     xnfne(j, 3) = xnfne(j, 3) * part(j, 3)
                     xnfne(j, 4) = xnfne(j, 4) * part(j, 4)
                     xnfne(j, 5) = xnfne(j, 5) * part(j, 5)
                     xnfne(j, 6) = xnfne(j, 6) * part(j, 6)
*
                     call pfsaha(j, 12, 6, 13, part)
                     xnfmg(j, 1) = xnfmg(j, 1) * part(j, 1)
                     xnfmg(j, 2) = xnfmg(j, 2) * part(j, 2)
                     xnfmg(j, 3) = xnfmg(j, 3) * part(j, 3)
                     xnfmg(j, 4) = xnfmg(j, 4) * part(j, 4)
                     xnfmg(j, 5) = xnfmg(j, 5) * part(j, 5)
                     xnfmg(j, 6) = xnfmg(j, 6) * part(j, 6)
*
                     call pfsaha(j, 14, 6, 13, part)
                     xnfsi(j, 1) = xnfsi(j, 1) * part(j, 1)
                     xnfsi(j, 2) = xnfsi(j, 2) * part(j, 2)
                     xnfsi(j, 3) = xnfsi(j, 3) * part(j, 3)
                     xnfsi(j, 4) = xnfsi(j, 4) * part(j, 4)
                     xnfsi(j, 5) = xnfsi(j, 5) * part(j, 5)
                     xnfsi(j, 6) = xnfsi(j, 6) * part(j, 6)
*
                     call pfsaha(j, 16, 6, 13, part)
                     xnfs(j, 1) = xnfs(j, 1) * part(j, 1)
                     xnfs(j, 2) = xnfs(j, 2) * part(j, 2)
                     xnfs(j, 3) = xnfs(j, 3) * part(j, 3)
                     xnfs(j, 4) = xnfs(j, 4) * part(j, 4)
                     xnfs(j, 5) = xnfs(j, 5) * part(j, 5)
                     xnfs(j, 6) = xnfs(j, 6) * part(j, 6)
*
                     call pfsaha(j, 26, 5, 13, part)
                     xnffe(j, 1) = xnffe(j, 1) * part(j, 1)
                     xnffe(j, 2) = xnffe(j, 2) * part(j, 2)
                     xnffe(j, 3) = xnffe(j, 3) * part(j, 3)
                     xnffe(j, 4) = xnffe(j, 4) * part(j, 4)
                     xnffe(j, 5) = xnffe(j, 5) * part(j, 5)
                  end do
*
               end if


!---------------------------------------------------------
!---------- write for debugging reasons------------------
!               do mm=1, nrhox
!               write (2,*) xnatom(mm), rhox(mm), rho(mm) 
!               rho(mm) = vwrho(mm)
!               end do


*
*.... ERASE FREQUENCY INTEGRALS
*
               if (ifcorr) call tcorr
               call ross
               call radiap
               if (nlteon) call stateq
            
*
*.... FREQUENCY INTEGRATION SECTION 
!           ( big do loop from nulo, nuhi)      !
*
               do nu = nulo, nuhi
!------------ if wave open ---------------------------------------
!----------------------------------------------------------------*
                  if (ifwave) then
*
                     if (wbegin .le. 1.0d10) then
                        wave = wbegin + dble(nu-nulo) * deltaw
                        freq = 2.997925d17 / wave
                        rco  = abs (deltaw / wave * freq)
*
                     else
*
*.... EQUALLY SPACED FREQUENCIES
*
                        freq = wbegin + dble(nu-nulo) * deltaw
                        rco  = deltaw
                     end if
*
                  else
                     freq = freset (nu)
                     rco  = rcoset (nu)
                  end if
!-------------------------------------------------------------------*
! --------  close if wave ------------------------------------------


                  freqlg = log10(freq)
                  freqln = log(freq)
                  freq15 = freq * 1.0d-15
                  waveno = freq / c_cm
*
                  do j = 1, nrhox
                     ehvkt(j)  = exp(-freq * hkt(j))
                     stim(j)   = 1.0d0 - ehvkt(j)
                     bnu(j)    = 1.47439d-2 * freq15**3 * ehvkt(j) /
     &                           stim(j)
                     dbnudt(j) = bnu(j) * freq * hkt(j) / t(j) / 
     &                           stim(j)
                     if(numnu .eq. 1) dbnudt(j) = 4.0d0 * sigma / pi * 
     &                                            t(j) ** 3
                  end do
*
!----------- opacity switches no 15 and no 16 -------------------
                  if (.not. ifop(15) .and. .not. ifop(16)) then
*
*.... THIS SECTION FOR NO METALLIC LINE BLANKETING
*
                     nsteps = 1
                     stepwt = 1.0d0
                     call kapp
*
                     if (ifop(14) .or. ifop(17) .or. ifop(18)) then
*
*.... FOR THE CASE OF HYDROGEN LINE BLANKETING
*
                        if (ifop(14)) call hlinop
*
*.... FOR THE CASE OF EXTRA LINE ABSORPTION
*
                        if (ifop(17)) call xlinop
*
*.... FOR THE CASE OF EXTRA LINE SCATTERING
*
                        if (ifop(18)) call xlisop
*
                        do j = 1, nrhox
                           aline(j) = ahline(j) + alines(j) + axline(j)
                           sline(j) = bnu(j)
                           if (aline(j) .gt. 0.0d0) sline(j) =
     &                       (ahline(j) * shline(j) + 
     &                        alines(j) * bnu(j) + 
     &                        axline(j) * sxline(j)) / aline(j)
                           sigmal(j) = siglin(j) + sigxl(j)
                        end do
*
                     end if
*
                     call josh
*
*.... CHECK FOR ZEROS
*
                     stopfl = .false.
*
                     do j = 1, nrhox
*
                        if (snu(j) .lt. 0.0d0) then
                           write (16, *) 'snu .lt. 0.0 at j =', j
                           stopfl = .true.
                        end if
*
                        if (jnu(j) .lt. 0.0d0) then
                           write (16, *) 'jnu .lt. 0.0 at j =', j
                           stopfl = .true.
                        end if
*
                        if (hnu(j) .lt. 0.0d0) then
                           write (16, *) 'hnu .lt. 0.0 at j =', j
                           stopfl = .true.
                        end if
*
c                        if (stopfl) stop
			if (stopfl) then
			   print *, ' snu, jnu or hnu < 0 at ', j
			   stop
			end if
                     end do
*
                     if (ifsurf .eq. 2) then
                        hnu(1) = surfi(1)
                        contin = surfi(1)
*
                     else
                        contin = hnu(1)
                     end if
*
                     put = contin
                     call outpt2
                     rcowt = rco * stepwt
*
                     if (ifsurf .eq. 0) then
                        if (ifcorr) call tcorr2 ((rcowt))
                        call radiap2 ((rcowt))
                        call ross2 ((rcowt))
                        if (nlteon) call stateq2 ((rcowt))
                     end if
*
*.... THIS PASSES VALUE OF STEPWT TO OUTPUT
*
                     put  = stepwt
                     iput = nsteps
                     call outpt3
                     call outpt4
*
!-------  opaciti switches else (15 on or  16 on):
!
!----------------------------------------------------
                  else
*
*.... THIS SECTION IS FOR THE LINE BLANKETED MODELS
*.... FIRST THE CONTINUUM OPACITIES
*
                     call kapp
*
*.... THIS NEXT LOOP IS NEEDED TO INITIALIZE ALINE, ... EACH NU
*
                     do j = 1, nrhox
                        aline(j)  = alines(j)
                        sline(j)  = bnu(j)
                        sigmal(j) = siglin(j) + sigxl(j)
                     end do
*
                     call josh
*
                     if (ifsurf .eq. 2) then
                        hnu(1) = surfi(1)
                        contin = surfi(1)
*
                     else
                        contin = hnu(1)
                     end if
*
                     put = contin
                     call outpt2
*
                     if (ifsurf .eq. 0) then
*
                        do j = 1, nrhox
                           abtotc(j)  = abtot(j)
                           alphac(j)  = alpha(j)
                           dtaunuc(j) = dtaunu(j)
                           hnuc(j)    = hnu(j)
                           jnuc(j)    = jnu(j)
                           residc(j)  = 0.0d0
                           snuc(j)    = snu(j)
                           taunuc(j)  = taunu(j)
                        end do
*
                     end if
*
                     sumwt = 0.0d0
*
*.... METALLIC LINE BLANKETING
*
                     finish = .false.
                     n = 0
*
                     do while (.not. finish)
                        n = n + 1
*
                        if (ifop(15) .and. lodf .eq. 'constant') then
			   if (freqid .eq. 'little') then
			      call linopl ((n), nsteps, stepwt)
			   else
			      call linopb ((n), nsteps, stepwt)
			   end if

*                          call linopc ((n), nsteps, stepwt)
*
                        else if (ifop(15) .and. 
     &                           lodf     .eq.  'memory  ') then
                           call linopm ((n), nsteps, stepwt)
*
                        else if (ifop(15) .and. 
     &                           lodf     .eq.  'variable') then
                           call linopv ((n), nsteps, stepwt)
                        end if
*
                        do j = 1, nrhox
                           aline(j)  = alines(j)
                           sline(j)  = bnu(j)
                           sigmal(j) = siglin(j) + sigxl(j)
                        end do
*
                        call josh


*
                        sumwt = sumwt + stepwt
                        rcowt = rco * stepwt
                        residc(1) = 1.0d0
*
                        if (ifsurf .eq. 0) then
*
                           do j = 1, nrhox
                              if (hnuc(j) .gt. 0.0d0) then
                                 residc(j) = hnu(j) / hnuc(j)
                              else
				  residc(j) = 1.0d0
                              end if
                           end do
*
                        else if (ifsurf .eq. 1 .and. 
     &                           contin .gt. 0.0d0) then
                           residc(1) = hnu(1) / contin
*
                        else if (ifsurf .eq. 2) then
                           hnu(1) = surfi(1)
                           if (contin .gt. 0.0d0) residc(1) = 
     &                                            surfi(1) / contin
                        end if
*
                        if (stepwt .gt. 0.0d0 .and. 
     &                      ifsurf .eq. 0) then
                           if (ifcorr) call tcorr2 ((rcowt))
                           call radiap2 ((rcowt))
                           call ross2 ((rcowt))
                           if (nlteon) call stateq2 ((rcowt))
                        end if
*
                        put  = stepwt
                        iput = nsteps
                        call outpt3
*
*.... CHECK TO SEE IF THIS FREQUENCY IS FINISHED
*
                        if (n .eq. nsteps) then
                           finish = .true.
*
                        else if (residc(1) .gt. 0.9995d0) then
                           finish = .true.
*
                        else if (residc(1) .gt. 0.998d0 .and. 
     &                           ifsurf    .gt. 0)    then
                           finish = .true.
*
                        else if (ifsurf .eq. 0) then
                           j = 1
                           do while (   j      .le. nrhox .and.
     &                               residc(j) .ge. 0.998d0)
                              j = j + 1
                           end do
                           if (j .gt. nrhox) finish = .true.
                        end if

                       if ( finish ) then 
!                        if (nu .eq. 532 ) then
!------------------ print debugging reason-------------------------!
!
!                        do j = 1, nrhox
!                        print*, 'acont,aline ',acont(j), aline(j)
!                        end do
! 
!                        do j= 1, nrhox
!                        print*,' abtot,sigmac ',abtot(j), sigmac(j)
!                        end do
!
!                        end if
                        
!!!!                        call ivwtest 
                       end if
*
                     end do
*
*.... FINISH OFF THIS FREQUENCY INTERVAL
*
                     residc(1) = 1.0d0
                     stepwt    = 1.0d0 - sumwt
                     if (stepwt .lt. 0.0001d0) stepwt = 0.0d0
*
                     if (ifsurf .eq. 0) then
*
                        do j = 1, nrhox
                           abtot(j)  = abtotc(j)
                           alpha(j)  = alphac(j)
                           dtaunu(j) = dtaunuc(j)
                           hnu(j)    = hnuc(j)
                           jnu(j)    = jnuc(j)
                           snu(j)    = snuc(j)
                           taunu(j)  = taunuc(j)
*
                           if (hnuc(j) .gt. 0.0d0) then
                              residc(j) = hnu(j) / hnuc(j)
                           else
                              residc(j) = 1.0d0
                           end if
*
                        end do
*
                     else if (ifsurf .eq. 1) then
                        hnu(1) = contin
*
                     else
                        surfi(1) = contin
                     end if
*
                     sumwt = sumwt + stepwt
                     rcowt = rco * stepwt
*
                     if (stepwt .gt. 0.0d0 .and. ifsurf .eq. 0) then
                        if (ifcorr) call tcorr2 ((rcowt))
                        call radiap2 ((rcowt))
                        call ross2 ((rcowt))
                        if (nlteon) call stateq2 ((rcowt))
                     end if
*
                     put  = stepwt
                     iput = nsteps
                     call outpt3
                     call outpt4
                  end if
!----------- end if for the opacity switches ----------------
!------------------------------------------------------------
*
               end do
!
!------------( close big do loop) ---------------------------*
*.... END OF THE FREQUENCY INTEGERATION
*
               if (ifsurf .eq. 0) then
*
*.... FINISH OFF THIS ITERATION
*
	print *, 'Final calls to ross '
                  call ross3
	print *, '	      to high '
                  call high
	print *, '	  and to radiap '
*
*.... FOLLOWING TWO LINES WERE REVERSED FOLLOWING BOB'S CODE OF 28 JUNE 1995
*
                  call radiap3
	print *, 'returned from radiap3 for ray  ', imod 

                  if (ifpres .and. ifconv) call convec
*
	print *, '     tidy up, tcorr, stateq and output '
                  if (ifcorr) call tcorr3
                  if (nlteon) call stateq3
                  if (ifturb) call turb
                  if (ifprnt .gt. 0) call outpt5
                  if (ifprnt .gt. 0) print *, ' We called outpt5'

               end if

!               if(ifpnch .gt. 0) then
!                  if (imod .eq. nmod) then
!                     close (unit = 41, status = 'keep')
!                  else
!                     close (unit = 41, status = 'delete')
!                  end if

!               else
!                  close (unit = 41, status = 'delete')
!               end if

*
            end do
!--END DO (2)---------------------------------------------*
*.... THE END OF LOOP OVER RAYS
*
         end if
*
      end do
!--END DO (1) --------------------------------------------
*
!----- tidy up ------------------------------------------
!--------------------------------------------------------
      close (unit = 15)
*
      if(ifprnt .gt. 0 .and. ifprnt .le. 4) then
         close (unit = 16, status = 'keep')
*
      else
         close(unit = 16, status = 'delete')
      end if
*
      if(ifpnch .gt. 0) then
         close (unit = 17, status = 'keep')
*....    close (unit = 41, status = 'keep')
*
      else
         close (unit = 17, status = 'delete')
*....    close (unit = 41, status = 'delete')
      end if
*
      if(ifprnt .eq. 5) then
         close (unit = 18, status = 'keep')
      else
         close (unit = 18, status = 'delete')
      end if
*
!         close (unit = 3, status = 'keep')



#endif
! end of the preprocessor flag ODF



      end
*
*********** E N D   O F   T H E  M A I N  P R O G R A M ****************
*
      subroutine deriv (x, f, dfdx, n)
      implicit none
*
*.... ASSUMES THAT ANY ZERO IN X OCCURS AT AN ENDPOINT
*
*--------------------------- DUMMY ARGUMENTS ---------------------------
*
      double precision  dfdx(*), f(*), x(*)
      integer  n
*
*---------------------------- LOCAL VARIABLES --------------------------
*
      double precision  d, d1, s, scale, tan, tan1
      integer j, n1
*
*------------------------------- EXECUTION -----------------------------
*
      dfdx(1) = (f(2) - f(1)) / (x(2) - x(1))
      n1 = n - 1
      dfdx(n) = (f(n) - f(n1)) / (x(n) - x(n1))
*
      if (n .gt. 2) then
         s = abs (x(2) - x(1)) / (x(2) - x(1))
*
         do j = 2, n1
            scale = max(abs(f(j-1)), abs(f(j)), abs(f(j+1))) / 
     &              abs(x(j))
            if (scale .eq. 0.0d0) scale = 1.0d0
            d1 = (f(j+1) - f(j)) / (x(j+1) - x(j)) / scale
            d =  (f(j)   - f(j-1)) / (x(j)   - x(j-1)) / scale
            tan1 = d1 / (s * sqrt(1.0d0 + d1 ** 2) + 1.0d0)
            tan =  d  / (s * sqrt(1.0d0 + d  ** 2) + 1.0d0)
            dfdx(j) = (tan1 + tan) / (1.0d0 - tan1 * tan) * scale
         end do
*
      end if
*
      end
*
*********** E N D   O F   S U B R O U T I N E   D E R I V E ************
*
      double precision function expi (n, x)
      implicit none
*
*.... EXPONENTIAL INTEGRAL FOR POSITIVE ARGUMENTS AFTER CODY AND
*.... THACHER, MATH. OF COMP.,22,641(1968)
*
*--------------------------- DUMMY ARGUMENTS ---------------------------
*
      double precision  x
      integer  n
*
*--------------------------- LOCAL VARIABLES --------------------------
*
      double precision 
     &         a0, a1, a2, a3, a4, a5, 
     &         b0, b1, b2, b3, b4,
     &         c0, c1, c2, c3, c4, c5, c6, 
     &         d1, d2, d3, d4, d5, d6, 
     &         e0, e1, e2, e3, e4, e5, e6, ex, ex1,
     &         f1, f2, f3, f4, f5, f6, 
     &         x1
      integer  i
      save     ex1, x1
*
*--------------------------- INITIALIZATION ---------------------------
*
      data x1 / -1.0d20 /
*
      data a0, a1, a2, a3, a4, a5 /
     &    -44178.5471728217d0,  57721.7247139444d0,  
     &      9938.31388962037d0,  1842.11088668000d0,   
     &       101.093806161906d0,    5.03416184097568d0 /
*
      data b0, b1, b2, b3, b4 /
     &       76537.3323337614d0, 32597.1881290275d0, 
     &        6106.10794245759d0,  635.419418378382d0, 
     &          37.2298352833327d0 /
*
      data c0, c1, c2, c3, c4, c5, c6 /
     &      4.65627107975096d-7, 0.999979577051595d0, 
     &      9.04161556946329d0, 24.3784088791317d0,    
     &     23.0192559391333d0,   6.90522522784444d0,
     &      0.430967839469389d0 /
*
      data d1, d2, d3, d4, d5, d6 /
     &   10.0411643829054d0, 32.4264210695138d0, 41.2807841891424d0,
     &   20.4494785013794d0,  3.31909213593302d0, 0.103400130404874d0/
*
      data e0, e1, e2, e3, e4, e5, e6 /
     &      -0.999999999998447d0, -26.6271060431811d0, 
     &    -241.055827097015d0,   -895.927957772937d0, 
     &   -1298.85688746484d0,    -545.374158883133d0,
     &      -5.66575206533869d0 /
*
      data f1, f2, f3, f4, f5, f6 /
     &      28.6271060422192d0,  292.310039388533d0, 
     &    1332.78537748257d0,   2777.61949509163d0, 
     &    2404.01713225909d0,    631.657483280800d0 /
*
*------------------------------- EXECUTION -----------------------------
*
      if (x .ne. x1) then
         x1 = x
         ex = dexp(-x1)
*
         if (x1 .gt. 4.0d0)  then
            ex1 = (ex + ex * (e0 + (e1 + (e2 + (e3 + (e4 + (e5 + e6 /
     &              x1) / x1) / x1) / x1) / x1) / x1) /
     &       (x1 + f1 +(f2 + (f3 + (f4 + (f5 + f6 / x1) / x1) / x1)
     &               / x1) / x1)) / x1
*
         else if (x1 .gt. 1.0d0) then
            ex1 = ex * (c6 + (c5 + (c4 + (c3 + (c2 + (c1 + c0 * x1) *
     &            x1) * x1) * x1) * x1) * x1) / (d6 + (d5 + (d4 +
     &            (d3 + (d2 + (d1 + x1) * x1) * x1) * x1) * x1) * x1)
*
         else if (x1 .gt. 0.0d0) then
            ex1 = (a0 + (a1 + (a2 + (a3 + (a4 + a5 * x1) * x1) * x1) *
     &               x1) * x1) / (b0 + (b1 + (b2 + (b3 + (b4 + x1) *
     &               x1) * x1) * x1) * x1) -dlog(x1)
*
         else
            ex1 = 0.0d0
         end if
*
      end if
*
      expi = ex1
*
      if (n .gt. 1) then
*
         do i = 1, n - 1
            expi = (ex - x1 * expi) / dble(i)
         end do
*
      end if
*
      end
*
*********** E N D   O F   F U N C T I O N    E X P I *******************
*
      subroutine high
      implicit none
*
*----------------------------- COMMONS ---------------------------------
*
      include 'common.sizebl'
      include 'common.abtotb'
      include 'common.height'
      include 'common.rhoxbl'
      include 'common.stateb'
*
*--------------------------- LOCAL VARIABLES --------------------------
*
      double precision  rhoinv(maxd)
      integer  j
*
*-------------------------------  EXTERNALS ----------------------------
*
      external integ
*
*------------------------------- EXECUTION -----------------------------
*
      do j = 1, nrhox
         rhoinv(j) = 1.0d-5 / rho(j)
      end do
*
      call integ (rhox, rhoinv, height, nrhox, 0.0d0)
      end
*
*********** E N D   O F   F U N C T I O N    H I G H *******************
*
      subroutine integ (x, f, fint, n, start)
      implicit none
*
*--------------------------- DUMMY ARGUMENTS ---------------------------
*
      double precision  f(*), fint(*), start, x(*)
      integer  n
*
*----------------------------- COMMONS ---------------------------------
*
      include 'common.sizebl'
      include 'common.parblk'
*
*--------------------------- LOCAL VARIABLES --------------------------
*
      double precision  db, df, wt
      integer           i, nn
*
*-------------------------------  EXTERNALS ----------------------------
*
      external parcoe
*
*------------------------------- EXECUTION -----------------------------
*
      nn = n
      call parcoe (f, x, (nn))
      fint(1) = start
      df      = x(2) - x(1)
      fint(2) = fint(1) + (a(1,1) + (b(1,1) * 0.5d0 + c(1,1) /
     &          3.0d0 * df) * df) * df
*
      do i = 2, nn - 2
         wt        = 1.0d0
         if (c(i,1) .ne. 0.0d0 .or. 
     &       c(i,2) .ne. 0.0d0 ) wt = abs(c(i,1)) / (abs(c(i,1)) + 
     &      abs(c(i,2)))
         df        = (x(i) - x(i+1))
         db        = -df
         fint(i+1) = fint(i) + wt * (a(i,2) + (b(i,2) * 0.5d0 + c(i,2)/
     &      3.0d0 * db) * db) * db - (1.0d0 - wt) * (a(i,1) + (b(i,1) *
     &      0.5d0 + c(i,1) / 3.0d0 * df) * df) * df
      end do
*
      df       = x(nn) - x(nn - 1)
      fint(nn) = fint(nn - 1) + (a(nn,1) + (b(nn,1) * 0.5d0 + c(nn,1) /
     &         3.0d0 * df) * df) * df
      end
*
*********** E N D   O F   S U B R O U T I N E   I N T E G **************
*
      integer function map1 (xold, fold, nold, xnew, fnew, nnew)
      implicit none
*
*--------------------------- DUMMY ARGUMENTS ---------------------------
*
      double precision  fnew(*), fold(*), xnew(*), xold(*)
      integer  nnew, nold
*
*--------------------------- LOCAL VARIABLES --------------------------
*
      double precision  ab, af, bb, bf, cb, cf, d, db, df, wt
      integer  l, ll, k, lm1, lm2, lp1
*
*------------------------------- EXECUTION -----------------------------
*
      l = 2
      ll = 0
*
      do k = 1, nnew
*
         do while (l .le. nold .and. xnew(k) .ge. xold(l))
            l = l + 1
         end do
*
         if (l .gt. nold) l = nold
*
         if (l .gt. 2 .and. l .lt. nold) then
*
*.... PARABOLIC CASE
*
            if (l .ne. ll) then
*
               if (l .gt. 3 .and. l .eq. ll+1) then
                  ab = af
                  bb = bf
                  cb = cf
*
               else
*
*.... MUST COMPUTE THE BACKWARD COEFFICIENTS
*
                  lm1 = l - 1
                  lm2 = l - 2
                  d = (fold(lm1) - fold(lm2)) /
     &                (xold(lm1) - xold(lm2))
                  cb = ((fold(l) - fold(lm1)) /
     &                  (xold(l) - xold(lm1)) - d) /
     &                 (xold(l) - xold(lm2))
                  bb = d + cb * (xold(lm1) - xold(lm2))
                  ab = fold(lm1)
               end if
*
               lp1 = l + 1
               lm1 = l - 1
               d = (fold(l) - fold(lm1)) / (xold(l) - xold(lm1))
               cf = ((fold(lp1) - fold(l)) / 
     &               (xold(lp1) - xold(l)) - d) /
     &              (xold(lp1) - xold(lm1))
               bf = d + cf * (xold(l) - xold(lm1))
               af = fold(l)
               wt = 0.0d0
               if (cf .ne. 0.0d0) wt = abs(cf) / (abs(cf) + abs(cb))
               ll = l
            end if
*
            df = xnew(k) - xold(l)
            db = xnew(k) - xold(lm1)
            fnew(k) = (1.0d0 - wt) * (af + (bf + cf * df) * df) + wt * 
     &                (ab + (bb + cb * db) * db)
*
         else
*
            if (l .ne. ll) then
               ll = l
               lm1 = l - 1
               af = fold(lm1)
               bf = (fold(l) - fold(lm1)) / (xold(l) - xold(lm1))
            end if
*
            fnew(k) = af + bf * (xnew(k) - xold(lm1))
         end if
*
      end do
*
      map1 = ll - 1
      end
*
*********** E N D   O F   F U N C T I O N   M A P 1 ********************
*
      subroutine output

      implicit none
*
*.... MODIFIED TO PRINT OUT <JUN(J)> FOR IFPRNT => 1
*.... 1993 JUL - MADE MAXMU A GLOBAL PARAMETER IN COMMON.SIZEBL
*.... 1994 JUL - FIXED BUG WITH DIMENSION AND EQUIVALENCE OF DUMMY
*                AND THE DIMENSION OF ABLOG.  THIS FORCED A CHANGE
*                IN THE WAY THAT THE OPACITIES ARE OUTPUT.
*.... 1995 OCT - EXPLICIT CONVERSION OF LOGICAL ifop TO INTEGER iop
*
*----------------------------- COMMONS ---------------------------------
*
      include 'common.sizebl'
      include 'common.abross'
      include 'common.abtotb'
      include 'common.convbl'
      include 'common.depart'
      include 'common.elembl'
      include 'common.fluxbl'
      include 'common.freqbl'
      include 'common.freset'
      include 'common.height'
      include 'common.ifblkk'
      include 'common.ifopbl'
      include 'common.ionsbl'
      include 'common.iterbl'
      include 'common.junkbl'
      include 'common.musblk'
      include 'common.opsblk'
      include 'common.optotb'
      include 'common.ptotal'
      include 'common.pzerob'
      include 'common.putblk'
      include 'common.radblk'
      include 'common.rhoxbl'
      include 'common.stateb'
      include 'common.taushj'
      include 'common.teffbl'
      include 'common.tempbl'
      include 'common.turbpr'
      include 'common.waveyb'
      include 'common.xabund'
*
*--------------------------- LOCAL VARIABLES --------------------------
*
      character  a*3, b*3
      double precision  ablog(37), contin, dummy(maxd, 37),
     &                  hlam, hlamlg, hlammg, hnulg, hnumg, hsurf, 
     &                  jbar(maxd), resid, stepwt, surfin(maxmu), 
     &                  tauend, wave
      integer    i, iop(20), iz, j, jtau1, n, nsteps, nu, mu
      logical    header
      save       contin, header, hsurf, jbar, n, nsteps, nu, 
     &           surfin, wave

      real*8 ::  disurfin, rw, mint

      real*8, allocatable, dimension(:) :: ip
*
*--------------------------- EQUIVALENCE -------------------------------
*
      equivalence (dummy(1,1), aal1(1))
*
*------------------------------ EXECUTION -------------------------------
*
*.... HEADINGS
*

      if(ifprnt .gt. 0) then
         header = .false.
*        nu = nulo - 1
         nu = 0
*
         if (ifpnch .ge. 2) then
!            write (17, 111) 'teff', teff, 'gravity', glog, wlte, 
!     &                      'title', title
111         format (a, f8.0, a9, f8.5, 1x, a / a6, a )
            if(ifsurf .eq. 2) then

!               write(17, '(A,/)') 'disk integrated and
!     $ mu-dependent intensity'

               write(17, 112) 'wvl', 'disk', (angle(mu), mu = 1, nmu)

112            format(4x,a3,9x,a4,11(5x,f7.2),/)
*
            else if (ifsurf .lt. 2) then
               write (17, 114) '#', 'wave', 'freq', 'flux', 
     &                         'cont', 'resid' 
114            format (a8, a10, a12, a14, a13, a13)
            end if
*
         end if
*
         if (ifprnt .eq. 5) write (18, 111) 'teff', teff, 
     &      'gravity', glog, wlte, 'title', title
*
      end if
*
      return
*
*----------------------- INITIALIZE SUMS OVER STEPS --------------------
*
      entry outpt2
*
*.... INITIALIZE <JBAR>
*
      do j = 1, nrhox
         jbar(j) = 0.0d0
      end do
*
      hsurf = 0.0d0
      wave  = 2.997925d17 / freq
      nu    = mod(nu, numnu-nulo+1)
      nu    = nu + 1
*
      do mu = 1, nmu
         surfin(mu) = 0.0d0
      end do
*
      n      = 0
      contin = put
      return
*
*---------------- SUM OVER STEPS AND STEP DEPENDENT QUANTITIES ---------
*
      entry outpt3
      n      = n + 1
      nsteps = iput
      stepwt = put
      hsurf  = hsurf + hnu(1) * stepwt
*
*.... SUM UP <JBAR>
*
      do j = 1, nrhox
         jbar(j) = jbar(j) + jnu(j) * stepwt
      end do
*
      do mu = 1, nmu
         surfin(mu) = surfin(mu) + surfi(mu) * stepwt
      end do
*
      if (ifprnt .gt. 0) then
*
         if (nsteps .gt. 1) then
*
            if (.not. header) then
               write (16, 101) 'wave(nm)', 'hlambda', 'log h',
     &            'mag', 'frequency', 'hnu', 'log h', 'mag',
     &            'resid', 'tauone taunu'
               header = .true.
            end if
*
            if(n .eq. 1 .and. ifprnt .gt. 1) write (16, 303) '0'
303         format (a)
*
            if(contin .gt. 0.0d0) then
               resid = hnu(1) / contin
*
            else
               resid = 1.0d0
            end if
*
            hnulg = log10(max(hnu(1), 1.0d-50))
            hnumg = -2.5d0 * hnulg
            jtau1 = 1
*
            do while (taunu(jtau1) .lt. 1.0d0)
               jtau1 = jtau1 + 1
            end do
*
            tauend = log10(taunu(nrhox))
            if(ifprnt .gt. 1) write (16, 305) stepwt, hnu(1), 
     &         hnulg, hnumg, resid, jtau1, tauend
305         format (57x, f10.8, 1pe14.4, 0pf12.5, f10.3, f9.5, i5, f8.2)
         end if
*
         if (ifprnt .eq. 3) then
            write (16, 312) 'wavelength(nm)', wave, 'frequency', freq,
     &         'rhox','taunu','abtot','alpha','Bnu','Snu','Jnu','Hnu',
     &         (j, rhox(j), taunu(j), abtot(j), alpha(j), bnu(j),
     &              snu(j), jnu(j), hnu(j), j = 1,nrhox)
312         format ( // 6x, a, f12.3, 3x, a, 1pe13.6 //
     &              12x, a, 7x, 3(a, 6x), 3(a, 8x), a //
     &              (6x, i2, 1p8e11.3))
*
         else if (ifprnt .eq. 4) then
            write (16, 320) 'opacities'
320         format (t55, a, /)
            write (16, 321) 
     &         (' ifop ', i = 1, 20), 
     &         (i, i = 1, 9), (i, i = 10, 20), 
     &         '  H1  ', '  H2+ ', '  H-  ', ' Hray ', '  He1 ', 
     &         '  He2 ', '  He- ', ' Heray', ' Cool ', ' Luke ', 
     &         '  Hot ', ' Elec ', ' H2ray', ' Hline', ' Lines', 
     &         ' Lscat', ' Xline', ' Xlsct', ' Xcont', ' Xscat'
321         format (8x, 20a6 / 5x, 9(5x, i1), 1x, 11(4x, i2) // 
     &              8x, 20a6 /)
*
            do j = 1, nrhox
*
               do i = 1, 37
                  ablog(i) = 0.0d0
                  if(dummy(j,i) .gt. 1.0d-99)ablog(i)=log10(dummy(j,i))
               end do
*
*..... THIS STRANGE ORDER IS NEEDED TO PULL THE CORRECT ENTRIES OUT OF
*..... THE ALPHABETIZED common.opsblk
*
               write (16, 326) j, 
*                   H1         H2+        H-         Hray       He1
     &           ablog(13), ablog(6),  ablog(11), ablog(32), ablog(7),  
*                   He2        He-       Heray       Cool      Luke
     &           ablog(8),  ablog(9),  ablog(34), ablog(3),  ablog(15),  
*                   Hot       Elec       H2ray       Hline     Lines
     &           ablog(12), ablog(31), ablog(33), ablog(10), ablog(14), 
*                  Lscat      Xline      Xlsct       Xcont     Xscat
     &           ablog(35), ablog(18), ablog(37), ablog(17), ablog(36),
     &           j
326            format (i4, t8, 20f6.2, 1x, i3)
            end do
*
         end if
*
      end if
      return
*
*-------------------------- PRINT SUMS OVER STEPS ----------------------
*
      entry outpt4
*
*.... OUTPUT <JBAR>
*
      if (ifprnt .eq. 5) then
         write(18, 450) 'frequency', nu, freq, 
     &                        '  wavelength(nm)', wave
450      format( a, i5, 1pe15.6, a, 0pf10.3 /)
         write(18, 451) ('depth', '<Jnu(j)>', j = 1, 4)
451      format(1x, a, 3x, a, 3(4x, a, 3x, a), /)
         write(18, 452) (j, jbar(j), j = 1, nrhox)
452      format((i4, 1pe14.4, 3(i6, e14.4)))
      end if
*
      if(nsteps .eq. 1) contin = hsurf
      if(contin .gt. 0.0d0) resid = hsurf / contin
      if(hsurf .le. 0.0d0) hsurf = 1.0d-50
*
      if(ifprnt .gt. 1) then
         jtau1 = 1
*
         do while (taunu(jtau1) .lt. 1.0d0)
            jtau1 = jtau1 + 1
         end do
*
         tauend = log10 (taunu(nrhox))
*
         if (nsteps .gt. 1) then
            jtau1  = 0
            tauend = 0.0d0
         end if
*
         if (ifsurf .le. 1) then
*
            if (.not. header) then
               header = .true.
               write (16, 101) 'wave(nm)', 'hlambda', 'log h',
     &            'mag', 'frequency', 'hnu', 'log h', 'mag',
     &            'resid', 'tauone taunu'
101            format ( //
     &                 12x, a, 4x, a, 2(7x, a), 4x, a, 8x, a, 10x, a,
     &                 7x, a, 4x, a, 2x, a)
            end if
*
            hlam   = hsurf * freq / wave
            hnulg  = log10 (hsurf)
            hlamlg = log10 (hlam)
            hlammg = -2.5d0 * hlamlg
            hnumg  = -2.5d0 * hnulg
            write (16, 401) nu, wave, hlam, hlamlg, hlammg, freq,
     &         hsurf,hnulg, hnumg, resid, jtau1, tauend, nu
401         format (/, i5, f14.3, 1pe13.4, 0pf12.5, f10.3, 1pe14.6,
     &              e13.4, 0pf12.5, f10.3, f9.5, i5, f8.2, i5)
*
         else if (ifsurf .eq. 2) then
*
            if( .not. header) then
               header = .true.
               write(6,102) 'wave(nm)', 'frequency', 
     &            'tauone taunu', 'mu  intensity', 'mu  intensity', 
     &            'mu  intensity', 'mu  intensity', 'mu  intensity'
102            format( /////10x, a, 5x, a, 3x, a, 5(4x, a))
            end if
*
            write (16, 406) nu, wave, freq, jtau1, tauend, 
     &                     (angle(mu), surfin(mu), mu = 1, nmu)
406         format (/, i5, f10.3, 1pe15.6, i6, 0pf6.2, 
     &              5(0pf7.4, 1pe10.3) / (42x, 5(0pf7.4, 1pe10.3)))
         end if
*
      end if
*
      if(ifpnch .ge. 2) then
*
         if (ifsurf .eq. 2) then

!            write (41, 441) wave
!441         format (f10.2)

!            write (17, 416) (surfin(mu), mu = 1, nmu)
!416         format ((1p8e11.3))

c            write (17, 416) nu, wave, freq,
c     &                      (surfin(mu), mu = 1, nmu)
c416         format (i5, f10.2, 1pe15.6, / (1p8e10.3))

            allocate(ip(nmu))

            ip = dsqrt(1.0d0 - angle(1 : nmu)**2.0d0)

            disurfin = 0.0d0

            do mu = 2, nmu

               rw = ip(mu)**2.0d0 - ip(mu - 1)**2.0d0

               mint = (surfin(mu) + surfin(mu - 1)) / 2.0d0

               disurfin = disurfin + mint * rw

            enddo

            deallocate(ip)

            write (17, 416) wave, disurfin, (surfin(mu), mu = 1, nmu)
416         format (es10.3,12(2x,es10.3))

*
         else if (ifsurf .lt. 2) then
            write (17, 411) 'flux', nu, wave, freq, hsurf, contin, 
     &                       resid
411         format (a, i5, f10.2, 1pe16.6, e13.4, e13.4, 0pf10.5)
            if(nu .eq. nuhi) write (17, 411) 'flux'
*
         end if
*
      end if
      return
*
*-------------------------------- SUMMARIES ----------------------------
*
      entry outpt5
*
      if (ifprnt .gt. 0) then
          write (16, *) 'DEBUG(OUT 1) ' 
         write (16, 501) 'rhox', 'ptotal', 'pturb', 'grdadb',
     &      'dltdlp', 'velsnd', 'dlrdlt', 'heatcp', 'hscale', 'vconv',
     &      'flxcnv',
     &      (j, rhox(j), ptotal(j), pturb(j), grdadb(j),
     &           dltdlp(j), velsnd(j), dlrdlt(j), heatcp(j),
     &           hscale(j), vconv(j), flxcnv(j), j = 1, nrhox)
501       format (/ 8x,a,7x,a,5x,a,6x,a,5x,a,4x,a,4(5x, a),6x,a,//
     &          (i3, 1p11e11.3))


!----------------------------------------------------------------
!          write (16, 501) 'rhox', 'ptotal', 'pturb', 'grdadb',
!     &      'dltdlp', 'velsnd', 'dlrdlt', 'heatcp', 'hscale', 'vconv',
!     &      'flxcnv'
!501          format( 11(a, 7x))
!
!          write (16, 5001) (j, rhox(j), ptotal(j), pturb(j), grdadb(j),
!     &           dltdlp(j), velsnd(j), dlrdlt(j), heatcp(j), 
!     &           hscale(j), vconv(j), flxcnv(j), j = 1, nrhox)
!5001     format (i3, 11 (1p11e11.3) //)
!
!-----------------------------------------------------------------

         write (16, *) ' DEBUG( OUT 2)'
         write (16, 502) 'flux', flux
502      format (a, 1pe12.4)
         write (16, 503) 'xnatom', 'raden', 'pradk', 'xnfph1',
     &      'xnfph2', 'xnfphe1', 'xnfphe2', 'xnfphe3', 'vturb', 
     &      (j, xnatom(j), raden(j), pradk(j), xnfph(j,1), xnfph(j,2), 
     &          xnfphe(j,1), xnfphe(j,2), xnfphe(j,3), vturb(j),
     &          flxcnv0(j), flxcnv1(j), j = 1, nrhox)
503      format ( // 4(6x, a), 5x, a, 3(4x, a), 5x, a, //
     &      (i3, 1p11e11.3))
         write (16, 99) 'pradk0', pradk0
99       format (a, 1pe12.4)
         write (16, 541) 'teff', teff, 'log g', glog, title,
     &      'ray ', iter
541      format ( // a5, f8.0, a8, f9.2, 8x, a74, 2x, a9, i3 //)
*
         if (.not. ifcorr) then
*
            do j = 1, nrhox
               flxrad(j) = flux - flxcnv(j)
            end do
*
         end if
*
         do j = 1, nrhox
            flxcnv(j) = flxcnv(j) / (flxcnv(j) + flxrad(j))
         end do
*
         write (16, 542) 
     &      'electron', 'rosseland', 'height', 'rosseland', 'fraction', 
     &                  'radiative', 'per cent',  'flux',
     &      'rhox', 'temp', 'pressure', 'number', 'density', 'mean',
     &              '(km)', 'depth', 'conv flux', 'acceleration', 
     &              'error', 'deriv',
     &      (j, rhox(j), t(j), p(j), xne(j), rho(j), abross(j), 
     &          height(j), tauros(j), flxcnv(j), accrad(j), flxerr(j), 
     &          flxdrv(j), j = 1, nrhox)
542   format (34x, a, 11x, a, 3x, a, 2x, a, 2x, a, 2x, a, 3x, a, 3x, a /
     &        7x, a,  6x, a, 3x, a, 3x, a, 3x, a, 4x, a, 7x, a, 5x, a, 
     &         3x, a,  1x, a, 3x, a, 3x, a //
     &       (i3, 1pe10.3, 0pf9.1, 1p7e10.3, e11.3, 0pf10.2, f8.2))
      end if
*
      if(ifpnch .gt. 0) then
*
*----------------------------------- PUNCHOUT --------------------------
*
         if (ifconv) then
            a = 'on'
         else
            a = 'off'
         end if
*
         if (ifturb) then
            b = 'on'
         else
            b = 'off'
         end if
*
         do i = 1, 20
            iop(i) = 0
            if(ifop(i)) iop(i) = 1
         end do
*
         write (17, 552) 'teff ', teff, 'gravity', glog, wlte,
     &      'title ', title,
     &      ' opacity ifop', iop,
     &      ' convection ', a, mixlth, ' turbulence ', b, 
     &      trbfdg, trbpow, trbsnd, trbcon,
     &      'abundance scale ', xscale, ' abundance change', 
     &                                  (iz, abund(iz), iz = 1, 2),
     &      (' abundance change',
     &      (iz, abund(iz), iz = i, i+5), i = 3, 98, 6),
     &       ' abundance change', (iz, abund(iz), iz = 99, 99)
552      format (a, f7.0, 2x, a, f8.5, 1x, a /
     &         a, a /
     &         a, 20i2 /
     &         a, a, f6.2, a, a, 4f6.2 /
     &         a, f9.5, a, 2(i2, f8.5) /
     &        (a, 6(i3, f7.2)))
*
*.... CHANGED 20 JULY 1995 TO CONFORM WITH BOB'S atlas9v.for;83 OF 20 JUNE 1995
*
         write (17, 554) 'read deck6', nrhox,
     &      ' rhox, t, p, xne, abross, accrad, vturb',
     &      (rhox(j), t(j), p(j), xne(j), abross(j), accrad(j), 
     &       vturb(j), flxcnv(j), vconv(j), j = 1, nrhox)
554      format (a, i3, a / (1pe15.8, 0pf9.1, 1p7e10.3))
         write (17, 555) 'pradk', pradk0
555      format (a, 1pe11.4)
         if (nlteon) write (17, 556) 
     &      'read departure coefficients', nrhox,
     &      ' rhox  bhyd 1-6  bmin',
     &      (rhox(j), (bhyd(j,i), i = 1, 6), bmin(j) , j = 1, nrhox)
556      format (a, i3, a, /
     &          (1pe11.4, 0p7f9.4))
*
         if (.not. ifwave) then
            write (17, 562) 'read frequencies', numnu,nulo,nuhi,freqid
562         format (a, 3i4, 3x, a)
            write (17, 563) (nu, freset(nu), rcoset(nu), nu = 1, numnu)
563         format (i5, 1p2e17.8, i5, 2e17.8)
         end if
         write (17, 571) 'begin',  'ray ', iter, ' completed '
571      format (a, 20x, a, i3, a)
      end if
*
      end
*
*********** E N D   O F   S U B R O U T I N E   O U T P U T ************
*
      subroutine parcoe (f, x, n)
      implicit none
*
*.... NOW ALWAYS IN DOUBLE PRECISION
*.... THIS HAS BEEN REWRITTEN IN THE SPIRIT OF THE U LONDON SUGGESTION
*.....  VAR(M,1 OR 2)  1=FORWARD INTERPOLATION, 2=BACKWARD INTERPOLATION
*
*--------------------------- DUMMY ARGUMENTS ---------------------------
*
      double precision  f(*), x(*)
      integer           n
*
*----------------------------- COMMONS ---------------------------------
*
      include 'common.sizebl'
      include 'common.parblk'
*
*--------------------------- LOCAL VARIABLES --------------------------
*
      double precision  d, dx
      integer           j, n1, jp2, jp1, jm1
*
*------------------------------- EXECUTION -----------------------------
*
      n1      = n - 1
      c(1, 1) = 0.0d0
      c(1, 2) = c(1, 1)
      b(1, 1) = (f(2) - f(1)) / (x(2) - x(1))
      b(1, 2) = b(1, 1)
      a(1, 1) = f(1)
      a(1, 2) = a(1, 1)
      c(n, 1) = 0.0d0
      c(n, 2) = c(n, 1)
      b(n, 1) = (f(n) - f(n1)) / (x(n) - x(n1))
      b(n, 2) = b(n, 1)
      a(n, 1) = f(n1)
      a(n, 2) = a(n, 1)
*
      if (n .gt. 2) then
*
         do j = 2, n1 - 1
            jp2     = j + 2
            jp1     = j + 1
            jm1     = j - 1
            d       = (f(jp1) - f(j)) / (x(jp1) - x(j))
            dx      = x(jp2) - x(j)
            c(j, 1) = (f(jp2) - f(jp1)) / (x(jp2) - x(jp1)) /
     &               dx - d / dx
            b(j, 1) = d + c(j,1) * (x(jp1) - x(j))
            a(j, 1) = f(jp1)
*
            if (j .eq. 2) then
*.... MUST COMPUTE THE BACKWARD COEFFICIENT WHEN J = 2
               d       = (f(2) - f(1)) / (x(2) - x(1))
               dx      = x(3) - x(1)
               c(j, 2) = (f(3) - f(2)) / (x(3) - x(2)) / dx - d / dx
               b(j, 2) = d + c(j,2) * (x(2) - x(1))
               a(j, 2) = f(2)
*
            else
               a(j, 2) = a(jm1, 1)
               b(j, 2) = b(jm1, 1)
               c(j, 2) = c(jm1, 1)
            end if
*
         end do
*
         a(n1, 1) = f(n)
         b(n1, 1) = -d
         c(n1, 1) = 0.0d0
         a(n1, 2) = f(n1)
         b(n1, 2) = b(n1-1, 1)
         c(n1, 2) = c(n1-1, 1)
      end if
*
      end
*
*********** E N D   O F   S U B R O U T I N E   P A R C O E ************
*
      subroutine radiap
      implicit none
*
*--------------------------- DUMMY ARGUMENTS ---------------------------
*.... ON SUBSEQUENT ENTRIES
*
      double precision  rcowt
*
*----------------------------- COMMONS -------------------------------
*
      include 'common.sizebl'
      include 'common.constb'
*
      include 'common.abtotb'
      include 'common.fluxbl'
      include 'common.pzerob'
      include 'common.radblk'
      include 'common.rhoxbl'
      include 'common.taushj'
*
*--------------------------- LOCAL VARIABLES --------------------------
*
      double precision  hflux(maxd)
      integer  j
      save  hflux
*
*-------------------------------  EXTERNALS ----------------------------
*
      external integ
*
*--------------------------- EXECUTION --------------------------------
*
*.... INITIALIZATION
*
      do j = 1, nrhox
         hflux(j)  = 0.0d0
         raden(j)  = 0.0d0
         accrad(j) = 0.0d0
      end do
*
      pradk0 = 0.0d0
      return
*
*---------------------- INTEGRATION OVER FREQUENCY ---------------------
*
      entry radiap2 (rcowt)
*
      do j = 1, nrhox
         accrad(j) = accrad(j) + abtot(j) * hnu(j) * rcowt
         hflux(j)  = hflux(j) + hnu(j) * rcowt
         raden(j)  = raden(j) + jnu(j) * rcowt
	if (hnu(j) .lt. 0 .or. jnu(j) .lt. 0) then
		print *, ' OUPS ', hnu(j), jnu(j), raden(j), rcowt
	end if
      end do
*
      pradk0 = pradk0 + knu(1) * rcowt
      return
*
*---------------------------- FINISH IT OFF ----------------------------
*
      entry radiap3
*
	print *, ' Entry 3 radiap '

      do j = 1, nrhox
         raden(j)  = raden(j)  * fourpi / c_cm
         accrad(j) = accrad(j) * fourpi / c_cm
*
*.... FUDGE TO KEEP MODEL FROM BLOWING UP WITH LARGE FLUX ERRORS
*
         if (hflux(j) .gt. flux) accrad(j) = accrad(j) * flux/hflux(j)
      end do
*
!------  debugging
!	print *, raden, accrad, prad

      pradk0 = pradk0 * fourpi / c_cm
      if (hflux(1) .gt. flux) pradk0 = pradk0 * flux / hflux(1)

      call integ (rhox, accrad, prad, (nrhox), (accrad(1) * rhox(1)))
        
!-------- debugging !
!	print *, 'pradk0, prad and accrad ', pradk0, prad, accrad

*
	print *, ' and calculating new radiation pressure '
      do j = 1, nrhox
         pradk(j) = prad(j) + pradk0
      end do
	print *, ' returning home '
	print *, ' finish pradk0  and pradk', pradk0, pradk
      return
*
      end
*
*********** E N D   O F   S U B R O U T I N E   R A D I A P ************
*
      subroutine ross
      implicit none
*
*--------------------------- DUMMY ARGUMENTS ---------------------------
*.... ON SUBSEQUENT ENTRIES
*
      double precision  rcowt
*
*----------------------------- COMMONS ---------------------------------
*
      include 'common.sizebl'
      include 'common.constb'
*
      include 'common.abross'
      include 'common.abtotb'
      include 'common.freset'
      include 'common.freqbl'
      include 'common.rhoxbl'
      include 'common.tempbl'
*
*--------------------------- LOCAL VARIABLES --------------------------
*
      integer  j
*
*-------------------------------  EXTERNALS ----------------------------
*
      external integ
*
*---------------------------- EXECUTION -------------------------------
*
*--------------------------- INITIALIZATION ---------------------------
*
      do j = 1, nrhox
         abross(j) = 0.0d0
      end do
*
      return
*
*----------------------- FREQUENCY INTEGRATION -------------------------
*
      entry ross2 (rcowt)
*
      do j = 1, nrhox
         abross(j) = abross(j) + dbnudt(j) / abtot(j) * rcowt
      end do
*
      return
*
*-------------------------------- COMPLETION ---------------------------
*
      entry ross3
*
      do j = 1, nrhox
         abross(j) = (4.0d0 * sigma / pi) * t(j) ** 3 / abross(j)
      end do
*
      call integ (rhox, abross, tauros, (nrhox), (abross(1)*rhox(1)))
      end
*
*********** E N D   O F   S U B R O U T I N E   R O S S ****************
*
      subroutine solvit (aa, ntrue, n, b)
      implicit none
*
*.... THIS REPLACES THE ROUTINE BOB HAD WHICH WAS BASED ON A
*.... GAUSS-JORDAN ELIMINATION SCHEME
*.... THIS SUBROUTINE INVERTS THE MATRIX A(N,N) AND RETURNS
*.... THE SOLUTION IN THE VECTOR B(N).  IT USES THE METHOD OF
*.... TRIANGULAR DECOMPOSITION DISCUSSED IN CHAPTER 9 OF
*.... "A FIRST COURSE IN NUMERICAL ANALYSIS" BY ANTHONY RALSTON.
*.... THE EQUATIONS HAVE THE FORM AX=B.
*
*--------------------------- DUMMY ARGUMENTS ---------------------------
*
      integer  ntrue, n
      double precision  aa(1:ntrue,1:ntrue), b(*)
*
*----------------------------- COMMONS ---------------------------------
*
      include 'common.sizebl'
*
*--------------------------- LOCAL VARIABLES --------------------------
*
      double precision  a(maxd, maxd), dd, stor(maxd)
      integer  l1, i, j, jj, pp, p(maxd)
*
*------------------------------- EXECUTION -----------------------------
*
      l1 = n
*
      do i = 1, l1
*
         do j = 1, l1
            a(j,i) = aa(j,i)
         end do
*
      end do
*
      do j = 1, l1
*
         do i = 1, l1
            stor(i) = a(i,j)
         end do
*
         if (j .gt. 1) then
*
            do jj = 1, j - 1
               pp = p(jj)
               a(jj,j) = stor(pp)
               stor(pp) = stor(jj)
*
               do i = jj + 1, l1
                  stor(i) = stor(i) - a(i,jj) * a(jj,j)
               end do
*
            end do
*
         end if
*
         dd = stor(j)
*
         do i = j, l1
*
            if (dabs(dd) .le. dabs(stor(i))) then
               p(j) = i
               a(j,j) = stor(i)
               dd = stor(i)
            end if
*
         end do
*
         if (j .ne. l1) then
            pp = p(j)
            stor(pp) = stor(j)
*
            do i = j + 1, l1
               a(i,j) = stor(i) / a(j,j)
            end do
*
         end if
*
      end do
*
      do i = 1, l1
         stor(i) = b(i)
      end do
*
      do i = 1, l1
         pp = p(i)
         b(i) = stor(pp)
         stor(pp) = stor(i)
*
         if (i .ne. l1) then
*
            do j = i + 1, l1
               stor(j) = stor(j) - b(i) * a(j,i)
            end do
*
         end if
*
      end do
*
      do i = l1, 1, -1
         b(i) = b(i) / a(i,i)
*
         if (i .gt. 1) then
*
            do j = 1, i - 1
               b(j) = b(j) - b(i) * a(j,i)
            end do
*
         end if
*
      end do
*
      end
*
*********** E N D   O F   S U B R O U T I N E   S O L V I T ************
*
      subroutine stateq
      implicit none
*
*--------------------------- DUMMY ARGUMENTS ---------------------------
*.... ON SUBSEQUENT ENTRIES
*
      double precision  rcowt
*
*      THE BOUND-BOUND COLLISION RATES WERE DERIVED FROM AN ANALYTIC FIT
*      TO THE CROSS SECTION CALCULATIONS OF BURKE, ORMONDE, AND WHITAKER,
*      PROC. PHYS. SOC., 1968, VOL 92, 319
*
*      THE CROSS SECTION USED (IN UNITS OF PI*A0**2) IS
*
*      QIJ = 4*FIJ*(EH/E0)**2*(LOG(E/E0)/(E/E0)+.148 /(E/E0)**6)
*
*      FIJ = OSCILLATOR STRENGTH
*      EH = GROUND STATE BINDING ENERGY
*      E0 = THRESHOLD ENERGY
*                                                   D M PETERSON MAY 1968
*
*----------------------------- COMMONS ---------------------------------
*
      include 'common.sizebl'
      include 'common.constb'
*
      include 'common.depart'
      include 'common.freqbl'
      include 'common.ionsbl'
      include 'common.iterbl'
      include 'common.rhoxbl'
      include 'common.stateb'
      include 'common.taushj'
      include 'common.tempbl'
*
*--------------------------- LOCAL VARIABLES --------------------------
*
      double precision  a(6,6), coulx, dqrad(maxd,6), dqrd(maxd), dt, 
     &                  expi, f(8,8), fre5, gik, hcont(6), hminbf, hvc, 
     &                  q, qassoc, qcharg, qcoll(8,8), qelect, 
     &                  qradik(maxd,6), qradki(maxd,6), qrdhmk(maxd), 
     &                  qrdkhm(maxd), sqrtt, rfrwt, right(6), rj, rje, 
     &                  rjedt, th, theta, told(maxd), x0, y, z
      integer  i, j, l, n
      save     told, qrdhmk, qrdkhm, dqrd, dqrad, qradki, qradik
*
*-------------------------------  EXTERNALS ----------------------------
*
      external coulx, solvit
*
*--------------------------- EQUIVALENCE -------------------------------
*.... TURN OFF FOR NOW
*      equivalence (a(1), taunu(1)), (qcoll(1), hnu(1))
*
*--------------------------- INITIALIZATION ---------------------------
*
      data f / 8*0.0d0, 0.4162d0, 7*0.0d0, 0.07910d0, 0.6408d0, 6*0.0d0, 
     &         0.02899d0, 0.1193d0, 0.8420d0, 5*0.0d0, 0.01394d0, 
     &         0.04467d0, 0.1506d0, 1.038d0,  4*0.0d0, 0.007800d0, 
     &         0.02209d0, 0.05585d0, 0.1794d0, 1.231d0, 3*0.0d0, 
     &         0.004814d0, 0.01271d0, 0.02768d0, 0.06551d0, 0.2070d0, 
     &         1.425d0, 2*0.0d0, 0.003184d0, 0.008037d0, 0.01604d0, 
     &         0.03229d0, 0.07455d0, 0.2340d0, 1.615d0, 0.0d0 /
*
*-------------------------------- EXECUTION ----------------------------
*
*.... ERASE FREQUENCY INTEGRALS
*
      do j = 1, nrhox
         told(j)   = t(j)
         qrdhmk(j) = 0.0d0
         qrdkhm(j) = 0.0d0
         dqrd(j)   = 0.0d0
      end do
*
      do i = 1, 6
*
         do j = 1, nrhox
            dqrad(j,i)  = 0.0d0
            qradki(j,i) = 0.0d0
            qradik(j,i) = 0.0d0
         end do
*
      end do
*
      return
*
*------------------------ FREQUENCY INTEGRALS --------------------------
*
      entry stateq2(rcowt)
      rfrwt = fourpi / h * rcowt / freq
      hvc = 2.0 * h * freq * (freq / c_cm) ** 2
*
      do n = 2, 6
         hcont(n) = coulx ((n), (freq), 1.0d0)
      end do
*
      fre5 = freq * 1.0d-5
*
      if (freq .ge. 2.111d14) then
         hminbf = 6.801d-20 + (5.358d-8 + (1.481d3 +
     &      (-5.519d12 + 4.808d21 / fre5) / fre5) / fre5) / fre5
*
      else if (freq .gt. 1.8259d14) then
         hminbf = 3.695d-16 + (-1.251d-1 + 1.052d13 / freq) / freq
*
      else
         hminbf = 0.0d0
      end if
*
      do j = 1, nrhox
         rj    = rfrwt * jnu(j)
         rje   = rfrwt * ehvkt(j) * (jnu(j) + hvc)
         rjedt = rje   * hkt(j)   * freq / t(j)
*
         do i = 2, 6
            qradik(j,i) = qradik(j,i) + hcont(i) * rj
            dqrad(j,i)  = dqrad(j,i)  + hcont(i) * rjedt
            qradki(j,i) = qradki(j,i) + hcont(i) * rje
         end do
*
         qrdhmk(j) = qrdhmk(j) + hminbf * rj
         dqrd(j) = dqrd(j) + hminbf * rjedt
         qrdkhm(j) = qrdkhm(j) + hminbf * rje
      end do
*
      return
*
*---------------------------------- THIRD ENTRY ------------------------
*
      entry stateq3
      if (ifprnt .gt. 0) write (16,201)
     &   'hminus statistical equilibrium', 'rhox', 'qelect', 'qassoc',
     &   'qcharg', 'qrdkhm', 'qrdhmk', 'bmin'
201    format ( ////// 36x, a, / 10x, a, 7x, a, 4(6x, a), 7x, a)
*
      do j = 1, nrhox
         dt        = t(j) - told(j)
         theta     = 5040.0d0 / t(j)
         qelect    = 10.0d0 ** (-8.7) * theta ** (1.5) * xne(j)
         qassoc    = 10.0d0 ** (-8.7) * 2.0 * bhyd(j,1) * xnfph(j,1)
         qcharg    = 10.0d0 ** (-7.4) * theta ** 0.333333 * xnfph(j,2)
         qrdkhm(j) = qrdkhm(j) + dqrd(j) * dt
         bmin(j)   = (qrdkhm(j) + qelect + qassoc + qcharg) /
     &               (qrdhmk(j) + qelect + qassoc + qcharg)
         write (16, 211) j, rhox(j), qelect, qassoc, qcharg, 
     &                      qrdkhm(j), qrdhmk(j), bmin(j)
211      format (i5, 1p6e12.3, 0pf10.4)
      end do
*
      if (ifprnt .gt. 1) write (16, 31) 
     &   'statistical equilibrium rates',
     &   'rate=sign(log10(max(abs(rate*1.0e20),1.0)),rate) ',
     &   '  rad   1-k   k-1   2-k   k-2   3-k   k-3',
     &        '   4-k   k-4   5-k   k-5   6-k   k-6',
     &   '  coll  1-k   2-k   3-k   4-k   5-k   6-k   5-8   6-8  ',
     &   '  coll  1-2   1-3   1-4   1-5   1-6   1-7   2-3   2-4  ',
     &           '2-5   2-6   2-7   3-4   3-5   3-6   3-7   4-5 ',
     &         '  4-6   4-7   5-6   5-7   6-7 '
31    format ( ////// 30x, a, 4x, a, /
     &        a, a, a, a, /
     &        a, a, a)
*
      do j = 1, nrhox
         dt    = t(j) - told(j)
         th    = hydip / tkev(j)
         sqrtt = sqrt (t(j))
*
         do i = 1, 7
            y = i
            qcoll(i,i) = 2.2d-8 * y**3 / sqrt(th) * exp(-th / y**2) *
     &                   xne(j)
*
*.... QCOLL(I,I) IS THE BOUND FREE RATE
*
            do l = i + 1, 8
               z = l
               gik = 1.0d0 / y**2 - 1.0d0 / z**2
               x0 = th * gik
               q = 2.186d-10 * f(i, l) / gik**2 * x0 * sqrtt *
     &                  (expi(1, (x0)) + 0.148d0 * x0 * expi(5, (x0)))
               qcoll(i, l) = q * xne(j)
               qcoll(l, i) = qcoll(i, l) * (y/z)** 2 * exp(x0)
            end do
*
         end do
*
         y = 8.0
         qcoll(8,8) = 2.2d-8 * y**3 / sqrt(th) * exp(-th / y**2) *
     &                xne(j)
*
         do i = 1, 6
            a(i,i) = qradik(j,i)
            qradki(j,i) = qradki(j,i) + dqrad(j,i) * dt
            right(i) = qradki(j,i) + qcoll(i,i) + qcoll(i,7) +
     &                 qcoll(i,8)
*
            do l = 1, 8
               a(i, i) = a(i, i) + qcoll(i, l)
            end do
*
            if (i .lt. 6) then
*
               do l = i + 1, 6
                  a(i, l) = -qcoll(i, l)
                  a(l, i) = -qcoll(l, i)
               end do
*
            end if
*
         end do
*
         call solvit (a, (6), (6), right)
*
         do i = 1, 6
            bhyd(j,i) = right(i)
         end do
*
         if (ifprnt .gt. 1) then
*
            do i = 1, 6
               qradki(j,i) = sign (log10 (
     &            max(abs(qradki(j,i) * 1.0d20),1.0d0)) , qradki(j,i))
               qradik(j,i) = sign (log10 (
     &            max(abs(qradik(j,i)*1.0d20),1.0d0)) , qradik(j,i))
            end do
*
            do i = 1, 8
*
               do l = 1, 8
                  qcoll(i, l) = sign (log10 (max(
     &               abs(qcoll(i, l) * 1.0d20),1.0d0)) , qcoll(i, l))
               end do
*
            end do
*
            write (16, 100) '0', j, (qradik(j,i), qradki(j,i), i = 1,6),
     &         (qcoll(i,i), i = 1, 6), qcoll(5,8), qcoll(6,8)
100         format (a, i5, 12f6.2, 6x, 8f6.2)
            write (16, 110) 
     &         (qcoll(1,l), l = 2, 7), (qcoll(2,l), l = 3, 7),
     &         (qcoll(3,l), l = 4, 7), (qcoll(4,l), l = 5, 7),
     &         (qcoll(5,l), l = 6, 7),  qcoll(6,7)
110       format (6x, 21f6.2)
         end if
*
      end do
*
      write (16, 170) 'statistical equilibrium for hydrogen',
     &   'rhox', 'b1', 'b2', 'b3', 'b4', 'b5', 'b6',
     &   (j, rhox(j), (bhyd(j,i), i = 1, 6), j = 1, nrhox)
170   format ( ////// 30x, a /
     &       15x, a, 10x, a, 5(8x, a) /
     &       (8x, i2, 1pe11.4 , 1x, 0p6f10.4))
      end
*
*********** E N D   O F   S U B R O U T I N E   S T A T E Q ************
*
      subroutine tcorr
      implicit none
*
*.... 10 JUNE 1993 VERSION
*.... 1995 JUL 20 - CHANGED TO CONFORM TO BOB'S atlas9v.for;83 BY
*                   ADDING LOCAL VARIABLE cccccc, AND A LOOP THAT USES IT
*
*--------------------------- DUMMY ARGUMENTS ---------------------------
*.... ON SUBSEQUENT ENTRIES
*
      double precision  rcowt
*
*----------------------------- COMMONS ---------------------------------
*
      include 'common.sizebl'
      include 'common.constb'
*
      include 'common.abross'
      include 'common.abtotb'
      include 'common.convbl'
      include 'common.depart'
      include 'common.fluxbl'
      include 'common.freset'
      include 'common.freqbl'
      include 'common.iterbl'
      include 'common.ptotal'
      include 'common.pzerob'
      include 'common.radblk'
      include 'common.rhoxbl'
      include 'common.stateb'
      include 'common.steplg'
      include 'common.taushj'
      include 'common.tcorrb'
      include 'common.teffbl'
      include 'common.tempbl'
      include 'common.turbpr'
*
*--------------------------- LOCAL VARIABLES --------------------------
*
      double precision ab1(maxd), ab2(maxd), cccccc(maxd), cnvfl, 
     &                 cnvflx(maxd), codrhx(maxd), d, dabros(maxd), 
     &                 dabtot(maxd), ddel(maxd), ddlt(maxd), del,
     &                 diagj, drhox(maxd), dtau(maxd), dtdrhx(maxd), 
     &                 dtflux(maxd), dtlamb(maxd), dtsur, dtsurf(maxd), 
     &                 dum(maxd), dum1(maxd), dum2(maxd), dum3(maxd),
     &                 dum4(maxd), dum5(maxd), dum6(maxd), dum7(maxd),
     &                 dum8(maxd), ex, expi, fluxco, g(maxd), 
     &                 gflux(maxd), hratio(maxd), 
     &                 hratio1, oldt1(maxd), p1(maxd), 
     &                 p2(maxd), ppp(maxd), prdnew(maxd), ptot1(maxd), 
     &                 ptot2(maxd), ratio, rdabh(maxd), rdiagj(maxd), 
     &                 rdiagjnu(maxd), rhoxstd, rjminsnu(maxd), 
     &                 rjmins(maxd), rrr(maxd), t1(maxd),
     &                 taustd(maxd), tav, teff25, term1, 
     &                 term2, tinteg(maxd), tnew1(maxd), tnew2(maxd), 
     &                 tone, tplus(maxd), ttwo, vco
      integer          i, idum, j, map1
*
      save     rdabh, rdiagj, rdiagjnu, rjmins, rjminsnu

*
*-------------------------------  EXTERNALS ----------------------------
*
      external deriv, integ, map1
*
*----------------------------- INITIALIZATION -------------------------
*
*.... VARIABLES FOR THE MIXING LENGTH FROM HENYEY ET AL
*.... THESE ARE NOT USED HERE FOR NOW.
*
      double precision  nuc, yc
      data  yc, nuc / 0.5d0, 8.0d0 /
*
*------------------------------- EXECUTION -----------------------------
*
*.... ERASE FREQUENCY INTEGRALS ON THE FIRST ENTRY
*
      do j = 1, nrhox
         flxrad(j) = 0.0d0
         rdabh(j)  = 0.0d0
         rdiagj(j) = 0.0d0
         rjmins(j) = 0.0d0
      end do
*
      return
*
*------------------ THE SECOND ENTRY = FREQUENCY INTEGRATION -----------
*
      entry tcorr2 (rcowt)
      call deriv (rhox, abtot, dabtot, (nrhox))
*
      do j = 1, nrhox
         flxrad(j)   = flxrad(j) + rcowt * hnu(j)
         rdabh(j)    = rdabh(j)  + rcowt * hnu(j) * dabtot(j) / 
     &                 abtot(j)
         rjminsnu(j) = abtot(j) * rcowt * jmins(j)
         rjmins(j)   = rjmins(j) + rjminsnu(j)
      end do
*
      term2 = 0.0d0
*
      do j = 1, nrhox
         term1 = term2
         d = max(dtaunu(j), 1.0d-10)
*
         if (d .gt. 0.01d0) then
            ex = 0.0d0
            if (d .lt. 10.0d0) ex = expi (3, (d))
            term2 = 0.5d0 * (d + ex - 0.5d0) / d
*
         else
            term2 = (0.922784335098467d0 - log(d)) * d / 4.0d0 + 
     &              d ** 2 / 12.0d0 - d ** 3 / 96.0d0 + d ** 4 / 
     &              720.0d0
         end if
*
         diagj = term1 + term2
         rdiagjnu(j) = abtot(j) * (diagj - 1.0d0) / 
     &                 (1.0d0 - alpha(j) * diagj) * 
     &                 (1.0d0 - alpha(j)) * dbnudt(j) * rcowt
         rdiagj(j) = rdiagj(j) + rdiagjnu(j)
      end do
*
      return
*
*------------------------------- ENTRY 3 -------------------------------
*
*.... AVRETT-KROOK TAU CORRECTION MODIFIED FOR CONVECTION
*
      entry tcorr3
      call deriv (rhox, t,      dtdrhx, (nrhox))
      call deriv (rhox, dltdlp, ddlt,   (nrhox))
      call deriv (rhox, abross, dabros, (nrhox))
*
      do j = 1, nrhox
         cnvflx(j) = 0.0d0
      end do
*
      if (ifconv) then
*
         do j = 3, nrhox
            cnvflx(j) = flxcnv(j)
         end do
*
      end if
*
*.... MODIFIED 20 JULY 1995
*.... CONFORMS WITH BOB'S atlas9v.for;83 OF 28 JUNE 1995
*
      do j = 2, nrhox-1
         cccccc(j) = 0.25d0 * (cnvflx(j-1) + 2.0d0 * cnvflx(j) +
     &                         cnvflx(j+1))
      end do
*
      do j = 2, nrhox-1
         cnvflx(j) = cccccc(j)
      end do
*
      do j = 1, nrhox
         rdabh(j) = rdabh(j) - flxrad(j) * dabros(j) / abross(j)
         del      = 1.0d0
         d        = 0.0d0
*
         if (cnvflx(j) .gt. 0.0d0 .and. flxcnv0(j) .gt. 0.0d0) then
            del    = dltdlp(j) - grdadb(j)
            vco    = 0.5d0 * mixlth * sqrt(max(-0.5d0 * ptotal(j) / 
     &               rho(j) * dlrdlt(j), 0.0d0))
            fluxco = 0.5d0 * rho(j) * heatcp(j) * t(j) * mixlth / fourpi
            if (mixlth .gt. 0.0d0 .and. vco .gt. 0.0d0) d = 8.0d0 * 
     &         5.6697d-5 * t(j) ** 4 / (abross(j) * hscale(j) * 
     &         rho(j)) / (fluxco * fourpi) / vco
            d       = d ** 2 * 0.5d0
            ddel(j) = (1.0d0 + d / (d + del)) / del
         end if
*
         cnvfl = 0.0d0
         if ( cnvflx(j) / flxrad(j) .gt. 0.001d0 .and. 
     &       flxcnv0(j) / flxrad(j) .gt. 0.001d0) cnvfl = cnvflx(j)
         codrhx(j) = (rdabh(j) + cnvfl * (dtdrhx(j) / t(j) * 
     &               (1.0d0 - 9.0d0 * d / (d + del)) + 
     &                1.5d0 * ddlt(j) / del * (1.0d0 + d / 
     &               (d + del)))) / (flxrad(j) + cnvflx(j) * 
     &                1.5d0 * dltdlp(j) / del * (1.0d0 + d / 
     &               (d + del)))
      end do
*
      codrhx(1) = 0.0d0
      codrhx(2) = 0.0d0
      call integ (rhox, codrhx, g, (nrhox), 0.0d0)
*
      do j = 1, nrhox
         g(j)     = exp(g(j))
         gflux(j) = g(j) * (flxrad(j) + cnvflx(j) - flux) / 
     &              (flxrad(j) + cnvflx(j) * 1.5d0 * dltdlp(j) * 
     &              ddel(j))
*
*BUG!    1.5*DLTDLP(J)/DEL*(1.+D/(D+DEL))) BUG FOUND BY U LONDON OBS
*
      end do
*
      call integ (tauros, gflux, dtau, (nrhox), 0.0d0)
*
      do j = 1, nrhox
         dtau(j)   = dtau(j) / g(j)
         dtau(j)   = max(-tauros(j) * 0.5d0, 
     &                   min(tauros(j) * 0.5d0, dtau(j)))
         dtflux(j) = -dtau(j) * dtdrhx(j) / abross(j)
      end do
*
      j = 1
*
      do while (j .le. nrhox .and. tauros(j) .lt. 0.03d0)
*
         do i = 1, j
            dtflux(i) = dtflux(i) * 0.5d0
         end do
*
         j = j + 1
      end do
*
      dtflux(1) = 0.0d0
      dtflux(2) = 0.0d0
*
      do j = 1, nrhox
         flxerr(j) = (flxrad(j) + cnvflx(j) - flux) / flux * 100.0d0
      end do
*
      call deriv (tauros, flxerr, flxdrv, (nrhox))
      teff25 = teff / 25.0d0
*
      do j = 1, nrhox
         if (cnvflx(j) / flxrad(j) .lt. 0.001d0) flxdrv(j) = rjmins(j) / 
     &      abross(j) / flux * 100.0d0

         dtlamb(j) = -flxdrv(j) * flux * 0.01d0 / rdiagj(j) * abross(j)
*
         if(cnvflx(j) / flxrad(j) .ge. 1.0d-5 .or. 
     &      tauros(j) .ge. 1.0d0)             then
            dtlamb(j)   = 0.0d0
            dtlamb(j-1) = dtlamb(j-1) * 0.5d0
            dtlamb(j-2) = dtlamb(j-2) * 0.5d0
            dtlamb(j-3) = dtlamb(j-3) * 0.5d0
            dtlamb(j-4) = dtlamb(j-4) * 0.5d0
            dtlamb(j-5) = dtlamb(j-5) * 0.5d0
         end if
*
*.... FUDGE TO AVOID VERY LARGE TEMPERATURE CORRECTIONS
*
         dtlamb(j) = max(-teff25, min(teff25, dtlamb(j)))
      end do
*
      dtsur = (flux - flxrad(1)) / flux * 0.25d0 * t(1)
      dtsur = max(-teff25, min(teff25, dtsur))
*
      do j = 1, nrhox
         dum(j) = dtflux(j) + dtlamb(j)
      end do
*
      call integ (tauros, dum, tinteg, (nrhox), 0.0d0)
      idum = map1 (tauros, tinteg, (nrhox), 0.1d0, tone, 1)
      idum = map1 (tauros, tinteg, (nrhox), 2.0d0, ttwo, 1)
      tav = (ttwo - tone) * 0.5d0
      if (dtsur * tav .le. 0.0d0) tav = 0.0d0
      if (abs(tav) .gt. abs(dtsur)) tav = dtsur
      dtsur = dtsur - tav
*
      do j = 1, nrhox
         hratio(j) = cnvflx(j) / (cnvflx(j) + flxrad(j))
      end do
*
      idum = map1 (tauros, hratio, (nrhox), 1.0d0, hratio1, 1)
*
      do j = 1, nrhox
         dtsurf(j) = dtsur * (1.0d0 - hratio1)
      end do
*
      do j = 1, nrhox
*
*.... REDUCE SURFACE CORRECTION TO THE FRACTION OF RADIATIVE FLUX
*
         dtsurf(j) = dtsurf(j) * (1.0d0 - hratio(j))
         t1(j) = dtflux(j) + dtlamb(j) + dtsurf(j)
      end do
*
      if (ifprnt .gt. 0)
     &   write (16, 100) 'rhox', 't', 'dtlamb', 'dtsurf',
     &      'dtflux', 't1', 'conv/total',  'error', 'deriv',
     &      (j, rhox(j), t(j), dtlamb(j), dtsurf(j), dtflux(j),
     &          t1(j), hratio(j), flxerr(j), flxdrv(j),
     &          j = 1, nrhox)
100      format ( /// 8x, a, 9x, a, 6x, a, 2(3x, a), 6x, a, 3x, a, 6x, //
     &   a, 5x, a, //
     &   (i3, 1pe12.4, 0pf10.1, 4f9.1, 1x, 1pe11.3, 1x, 0p2f10.3))
*
      do j = 1, nrhox
*
         if(.not. ifconv .or. hratio(j) .le. 0.0d0) then
*
            if(.not. ifconv .or. j .lt. nrhox / 2) then
*
               if (iter .gt. 1) then
*
                  if (oldt1(j) * t1(j) .gt. 0.0d0 .and.
     &                abs(oldt1(j)) .gt. abs(t1(j)) ) t1(j) = t1(j) * 
     &                1.25d0
                  if(oldt1(j) * t1(j) .lt. 0.0d0) t1(j) = t1(j) * 
     &                0.5d0
               end if
*
            end if
*
         end if
*
        oldt1(j) = t1(j)
      end do
*
*.... DETERMINE RHOX CORRECTION TO MAINTAIN CONSTANT TAUROS
*
      do j = 1, nrhox
         tplus(j)  = t(j) + t1(j)
         taustd(j) = 10.0d0 ** (tau1lg + dble(j-1) * steplg)
      end do
*
      if(tauros(1) .gt. taustd(1)) then
         tauros(1) = taustd(1)
         rhoxstd   = min(taustd(1) / abross(1), rhox(1))
         ratio     = rhoxstd / rhox(1)
         p(1)      = ratio * p(1)
         prad(1)   = ratio * prad(1)
         xne(1)    = ratio * xne(1)
         rhox(1)   = rhoxstd
      end if
*
      idum = map1 (tauros, t,    (nrhox), taustd, tnew1,  (nrhox))
      idum = map1 (tauros, prad, (nrhox), taustd, prdnew, (nrhox))
      call ttaup (tnew1, taustd, ab1, ptot1, p1, prdnew, pturb, 
     &            vturb, (grav), (nrhox))
      idum = map1 (tauros, tplus, (nrhox), taustd, tnew2, (nrhox))
      call ttaup (tnew2, taustd, ab2, ptot2, p2, prdnew, pturb, 
     &            vturb, (grav), (nrhox))
*
      do j = 1, nrhox
         ppp(j) = (ptot2(j) - ptot1(j)) / ptot1(j)
      end do
*
      idum = map1 (taustd, ppp, (nrhox), tauros, rrr, (nrhox))
*
      do j = 1, nrhox
         drhox(j) = rrr(j) * rhox(j)
      end do
*
      drhox(1) = 0.0d0
      drhox(2) = 0.0d0
      drhox(3) = 0.0d0
      drhox(4) = drhox(4) / 16.0d0
      drhox(5) = drhox(5) /  8.0d0
      drhox(6) = drhox(6) /  4.0d0
      drhox(7) = drhox(7) /  2.0d0
*
      do j = 1, nrhox
         t(j) = t(j) + t1(j)
      end do
*
*.... FORCE MONOTONICITY
*
      do j = nrhox - 1, 1, -1
         t(j) = min(t(j), t(j+1) - 1.0d0)
      end do
*
*.... MINIMUM TEMPERATURE
*
      do j = 1, nrhox
         t(j) = max(t(j), 2089.0d0)
      end do
*
*.... CHANGE RHOX TO MAINTAIN CONSTANT TAUROS
*
      do j = 1, nrhox
         rhox(j) = rhox(j) + drhox(j)
      end do
*
      idum = map1(tauros, rhox,   (nrhox), taustd, dum1, (nrhox))
      idum = map1(tauros, t,      (nrhox), taustd, dum2, (nrhox))
      idum = map1(tauros, p,      (nrhox), taustd, dum3, (nrhox))
      idum = map1(tauros, xne,    (nrhox), taustd, dum4, (nrhox))
      idum = map1(tauros, abross, (nrhox), taustd, dum5, (nrhox))
      idum = map1(tauros, prad,   (nrhox), taustd, dum6, (nrhox))
      idum = map1(tauros, vturb,  (nrhox), taustd, dum7, (nrhox))
      idum = map1(tauros, bmin,   (nrhox), taustd, dum8, (nrhox))
*
      do j = 1, nrhox
         rhox(j)   = dum1(j)
         t(j)      = dum2(j)
         tk(j)     = k * t(j)
         hckt(j)   = hc / tk(j)
         hkt(j)    = h / tk(j)
         tkev(j)   = k_ev * t(j)
         tlog(j)   = log(t(j))
         p(j)      = dum3(j)
         xne(j)    = dum4(j)
         abross(j) = dum5(j)
         prad(j)   = dum6(j)
         pradk(j)  = prad(j) + pradk0
         vturb(j)  = dum7(j)
         bmin(j)   = dum8(j)
      end do
*
      do i = 1, 6
         idum = map1(tauros, bhyd(1,i), (nrhox), taustd, dum1, 
     &               (nrhox))
*
         do j = 1, nrhox
            bhyd(j, i) = dum1(j)
         end do
*
      end do
*
      do j = 1, nrhox
         tauros(j) = taustd(j)
      end do
*
      end
*
*********** E N D   O F   S U B R O U T I N E   T C O R R **************
*
      subroutine turb
      implicit none
*
*----------------------------- COMMONS ---------------------------------
*
      include 'common.sizebl'
      include 'common.convbl'
      include 'common.rhoxbl'
      include 'common.stateb'
      include 'common.turbpr'
*
*--------------------------- LOCAL VARIABLES --------------------------
*
      integer j
*
*------------------------------- EXECUTION -----------------------------
!--------------------debugging prints ----------------------------------*
!	print *, 'subroutine turb.. trbfdg, trbpow, trbsnd, trbcon ',
!     &		trbfdg, trbpow, trbsnd, trbcon
!	print *, ' rho, velsnd, vturb ', rho, velsnd, vturb

      do j = 1, nrhox
         vturb(j) = (trbfdg * rho(j) ** trbpow + trbsnd * velsnd(j) /
     &      1.0d5 + trbcon) * 1.0d5
         pturb(j) = rho(j) * vturb(j) ** 2 * 0.5d0
      end do
*
      end
*
*********** E N D   O F   S U B R O U T I N E   T U R B ****************

      logical function isinf(a)

      implicit none 

      double precision  a

      if ((a*0.d0).ne.0) then
       isinf = .true.
      else
      isinf = .false.
      end if

      return

      end

*********************************************************************** 

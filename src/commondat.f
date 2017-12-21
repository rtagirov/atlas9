      module commondat

      implicit none


      double precision vwrho(maxd), abross(maxd), tauros(maxd)
!!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   A B R O S S !!!!!!!!!!!!!!!!!
      double precision  abtot(maxd), alpha(maxd), dkappa(maxd)
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   A B T O T B !!!!!!!!!!!!!!!!!
!
!
!.... THE VARIOUS FUNDAMENTAL CONSTANTS
!.... VALUES TAKEN FROM K. R. LANG, ASTROPHYSICAL DATA
!
!.... 1995 OCT - REORGANIZED TO REMOVE CONSTANTS THAT INVOLVED DIVISION 
!     OF CONSTANTS
!
      double precision  amu, c_cm, c_nm, fourpi, h, hc, he2lim,  
     &                   he3lim, hydip, k, k_ev, pi, ryd, sige,  
     &                   sigma, tenlog
      parameter ( amu    = 1.6605402d-24,
     &            c_cm   = 2.99792458d10,
     &            c_nm   = c_cm ! 1.0d7,
     &            h      = 6.6260755d-27, 
     &            hc     = h ! c_cm,
     &            he2lim = 527490.06d0,
     &            he3lim = 588451.59d0,
     &            hydip  = 13.595d0,
     &            k      = 1.380658d-16, 
     &            k_ev   = 8.61708d-5,
     &            pi     = 3.1415926536d0,
     &            fourpi = 4.0d0 ! pi,
     &            ryd    = 3.28805d15,
     &            sige   = 6.65246163d-25,
     &            sigma  = 5.67051d-5,
     &            tenlog = 2.30258509299405d0)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   C O N S T B !!!!!!!!!!!!!!!!!
!
!.... 
      double precision abtotc(maxd), alphac(maxd), dtaunuc(maxd), 
     &                 hnuc(maxd), jnuc(maxd), residc(maxd), 
     &                 snuc(maxd), taunuc(maxd)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   C O N T B L !!!!!!!!!!!!!!!!!
!
!.... 
      double precision dlrdlt(maxd), dltdlp(maxd), flxcnv(maxd), 
     &                 flxcnv0(maxd), flxcnv1(maxd), grdadb(maxd), 
     &                 heatcp(maxd), hscale(maxd), mixlth, overwt, 
     &                 vconv(maxd), velsnd(maxd)
      logical          ifconv
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   C O N V B L !!!!!!!!!!!!!!!!!
!
!.... 
      double precision  bal1(maxd, 9),  bal2(maxd, 1),  bb1(maxd, 7), 
     &                  bc1(maxd, 14),  bc2(maxd, 6),   bca1(maxd, 8), 
     &                  bca2(maxd, 8),  bfe1(maxd, 15), bhe1(maxd, 29), 
     &                  bhe2(maxd, 6),  bhyd(maxd, 8),  bk1(maxd, 8), 
     &                  bmg1(maxd, 11), bmg2(maxd, 6),  bmin(maxd),
     &                  bna1(maxd, 8),  bo1(maxd, 13),  bo2(maxd, 4),
     &                  bsi1(maxd, 11), bsi2(maxd, 10) 
      logical           nlteon
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   D E P A R T !!!!!!!!!!!!!!!!!
!
!....
      double precision  edens(maxd)
      logical           ifedns
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   E D E N B L !!!!!!!!!!!!!!!!!
!
!.... 
      double precision abund(99), atmass(99)
      character        elem(99)!2
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   E L E M B L !!!!!!!!!!!!!!!!!
!
!.... 
      double precision flux, flxdrv(maxd), flxerr(maxd), flxrad(maxd)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   F L U X B L !!!!!!!!!!!!!!!!!
!
!
!.... ADDED dbnudt AND waveno
!.... 1995 JUL - ADDED freqln = log(freq), AND NOW freqlg = log10(freq)
!
      double precision bnu(maxd), dbnudt(maxd), dstim(maxd),
     &                 ehvkt(maxd), freq, freqlg, freqln, stim(maxd), 
     &                 waveno
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   F R E Q B L !!!!!!!!!!!!!!!!!
!
!.... 
      integer          nulo, nuhi, numnu
      double precision freset(maxnu), rcoset(maxnu)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   F R E S E T !!!!!!!!!!!!!!!!!
!
!.... 
      double precision height(maxd)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   H E I G H T !!!!!!!!!!!!!!!!!
!
!.... 
!.... 1993 JUL - ADDED VARIABLE ifmasks
!
      integer         ifsurf
      logical         ifcorr, ifmasks, ifmol, ifpres
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   I F B L K K !!!!!!!!!!!!!!!!!
!
!.... 
      integer           idequa(maxmeq), ifequa(101), kcomps(maxloc), 
     &                  locj(maxmol + 1), nequa, nloc, nummol
      double precision  code(maxmol), equil(7, maxmol)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   I F E Q U A !!!!!!!!!!!!!!!!!
!
!.... 
      character       lodf!8
      logical         ifop(20)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   I F O P B L !!!!!!!!!!!!!!!!!
!
!.... 
      double precision xnfh(maxd, 2), xnfhe(maxd, 3), 
     &                 xnfph(maxd,2), xnfphe(maxd,3)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   I O N S B L !!!!!!!!!!!!!!!!!
!
!.... 
      integer         iter, ifprnt, ifpnch, numits 
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   I T E R B L !!!!!!!!!!!!!!!!!
!
!....
      character	       title!74
      character        freqid!6
      character        wlte!4
      double precision xscale
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   J U N K B L !!!!!!!!!!!!!!!!!
!.... 
      double precision  dp, eh(maxd), ej(maxd), ek(maxd), eps(maxd), pl
      integer           ndepth
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   M E U D B L !!!!!!!!!!!!!!!!!
!
!.... 
      integer          nmu
      double precision angle(maxmu), surfi(maxmu)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   M U S B L K !!!!!!!!!!!!!!!!!
!
      double precision  aal1(maxd), ac1(maxd), acool(maxd), afe1(maxd), 
     &                  amg1(maxd), ah2p(maxd), ahe1(maxd), ahe2(maxd), 
     &                  ahemin(maxd), ahline(maxd), ahmin(maxd), 
     &                  ahot(maxd), ahyd(maxd), alines(maxd), 
     &                  aluke(maxd), asi1(maxd), axcont(maxd), 
     &                  axline(maxd), 
!
     &                  sal1(maxd), sc1(maxd), sfe1(maxd), smg1(maxd), 
     &                  she1(maxd), she2(maxd), shline(maxd), 
     &                  shmin(maxd), shyd(maxd), ssi1(maxd), 
     &                  sxcont(maxd), sxline(maxd), 
!
     &                  sigel(maxd), sigh(maxd), sigh2(maxd), 
     &                  sighe(maxd), siglin(maxd), sigx(maxd),  
     &                  sigxl(maxd)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   O P S B L K !!!!!!!!!!!!!!!!!
!
!.... 
      double precision acont(maxd), aline(maxd), scont(maxd), 
     &                 sigmac(maxd), sigmal(maxd), sline(maxd)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   O P T O T B !!!!!!!!!!!!!!!!!
!
!.... 
      double precision  a(maxd,2), b(maxd,2), c(maxd,2)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   P A R B L K !!!!!!!!!!!!!!!!!
!
!.... 
      double precision ptotal(maxd)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   P T O T A L !!!!!!!!!!!!!!!!!
!
!.... 
      integer          iput
      double precision put
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   P U T B L K !!!!!!!!!!!!!!!!!
!
!.... 
      double precision knu(maxd), pcon, pradk(maxd), pradk0, pturb0, 
     &                 pzero, raden(maxd)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   P Z E R O B !!!!!!!!!!!!!!!!!
!
!.... 
      double precision accrad(maxd), prad(maxd)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   R A D B L K !!!!!!!!!!!!!!!!!
!
!.... 
      integer           nrhox
      double precision  rhox(maxd)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   R H O X B L !!!!!!!!!!!!!!!!!
!
!
!.... 1996 Apr - CHANGED maxmol, maxmeq, maxloc TO CONFORM TO
!                BOB'S VALUES IN XNFPELSYN DATED 3Jan95
!
!.... maxd   = maximum number of depths in the atmosphere
!.... maxmeq = maximum number of molecular equations
!.... maxloc = maximum number of molecular components
!.... maxmol = maximum number of molecules
!.... maxmu  = maximum number of angles for the radiation field
!.... maxnu  = the maximum number of frequencies
!
      integer maxd, maxloc, maxmeq, maxmol, maxmu, maxnu
      parameter (maxd   = 256,
     &           maxloc = 600,
     &           maxmeq = 30,
     &           maxmol = 200,
     &           maxmu  = 20,
     &           maxnu  = 1563)
!
!!!!!!!!!!!!!!!!!!!!! E N D  O F   S I Z E B L O C K !!!!!!!!!!!!!!!!!!!!!
!
!
!.... 1996 Apr - CHANGED maxmol, maxmeq, maxloc TO CONFORM TO
!                BOB'S VALUES IN XNFPELSYN DATED 3Jan95
!
!.... maxd   = maximum number of depths in the atmosphere
!.... maxmeq = maximum number of molecular equations
!.... maxloc = maximum number of molecular components
!.... maxmol = maximum number of molecules
!.... maxmu  = maximum number of angles for the radiation field
!.... maxnu  = the maximum number of frequencies
!
      integer maxd, maxloc, maxmeq, maxmol, maxmu, maxnu
      parameter (maxd   = 128,
     &           maxloc = 600,
     &           maxmeq = 30,
     &           maxmol = 200,
     &           maxmu  = 20,
     &           maxnu  = 1563)
!
!!!!!!!!!!!!!!!!!!!!! E N D  O F   S I Z E B L O C K !!!!!!!!!!!!!!!!!!!!!
!
!
!.... 1996 Apr - CHANGED maxmol, maxmeq, maxloc TO CONFORM TO
!                BOB'S VALUES IN XNFPELSYN DATED 3Jan95
!
!.... maxd   = maximum number of depths in the atmosphere
!.... maxmeq = maximum number of molecular equations
!.... maxloc = maximum number of molecular components
!.... maxmol = maximum number of molecules
!.... maxmu  = maximum number of angles for the radiation field
!.... maxnu  = the maximum number of frequencies
!
      integer maxd, maxloc, maxmeq, maxmol, maxmu, maxnu
      parameter (maxd   = 256,
     &           maxloc = 600,
     &           maxmeq = 30,
     &           maxmol = 200,
     &           maxmu  = 20,
     &           maxnu  = 1563)
!
!!!!!!!!!!!!!!!!!!!!! E N D  O F   S I Z E B L O C K !!!!!!!!!!!!!!!!!!!!!
!
!
!.... 1996 Apr - CHANGED maxmol, maxmeq, maxloc TO CONFORM TO
!                BOB'S VALUES IN XNFPELSYN DATED 3Jan95
!
!.... maxd   = maximum number of depths in the atmosphere
!.... maxmeq = maximum number of molecular equations
!.... maxloc = maximum number of molecular components
!.... maxmol = maximum number of molecules
!.... maxmu  = maximum number of angles for the radiation field
!.... maxnu  = the maximum number of frequencies
!
      integer maxd, maxloc, maxmeq, maxmol, maxmu, maxnu
      parameter (maxd   = 96,
     &           maxloc = 600,
     &           maxmeq = 30,
     &           maxmol = 200,
     &           maxmu  = 20,
     &           maxnu  = 1563)
!
!!!!!!!!!!!!!!!!!!!!! E N D  O F   S I Z E B L O C K !!!!!!!!!!!!!!!!!!!!!
!
!
!.... 1996 Apr - CHANGED maxmol, maxmeq, maxloc TO CONFORM TO
!                BOB'S VALUES IN XNFPELSYN DATED 3Jan95
!
!.... maxd   = maximum number of depths in the atmosphere
!.... maxmeq = maximum number of molecular equations
!.... maxloc = maximum number of molecular components
!.... maxmol = maximum number of molecules
!.... maxmu  = maximum number of angles for the radiation field
!.... maxnu  = the maximum number of frequencies
!
      integer maxd, maxloc, maxmeq, maxmol, maxmu, maxnu
      parameter (maxd   = 72,
     &           maxloc = 600,
     &           maxmeq = 30,
     &           maxmol = 200,
     &           maxmu  = 20,
     &           maxnu  = 1563)
!
!!!!!!!!!!!!!!!!!!!!! E N D  O F   S I Z E B L O C K !!!!!!!!!!!!!!!!!!!!!
!
!.... 
      double precision p(maxd), rho(maxd), xnatom(maxd), xne(maxd)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   S T A T E B !!!!!!!!!!!!!!!!!
!
!.... 
      double precision  steplg, tau1lg
      integer           krhox
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   S T E P L G !!!!!!!!!!!!!!!!!
!
!....
      double precision  tabkap(56,38,5), tabp(38), tabt(56), tabv(5)
      integer           np, nt, nv
      logical           iftabk
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   T A B P T V !!!!!!!!!!!!!!!!!
!
!....
!.... 1994 JAN - MODIFIED TO ADD jmins
!.... 
      double precision  hnu(maxd), jmins(maxd), jnu(maxd), snu(maxd), 
     &                  taunu(maxd)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   T A U S H J !!!!!!!!!!!!!!!!!
!
!.... 
      double precision  ddtau(maxd), dtaunu(maxd), eddfac(maxd), fh, 
     &                  kappa(maxd), ratmab(maxd), ratmop(maxd)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   T C O R R B !!!!!!!!!!!!!!!!!
!
!.... 
      double precision glog, grav, teff
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   T E F F B L !!!!!!!!!!!!!!!!!
!
!.... 
      integer          itemp
      double precision hckt(maxd), hkt(maxd), t(maxd), tk(maxd), 
     &                 tkev(maxd), tlog(maxd)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   T E M P B L !!!!!!!!!!!!!!!!!
!
!.... 
      logical          ifturb
      double precision pturb(maxd), trbcon, trbfdg, trbpow, trbsnd, 
     &                 vturb(maxd)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   T U R B P R !!!!!!!!!!!!!!!!!
!
!.... 
      logical          ifwave
      double precision deltaw, wbegin
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   W A V E Y B !!!!!!!!!!!!!!!!!
!
!.... 
      logical          ifwave
      double precision deltaw, wbegin


!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   X A B U N D !!!!!!!!!!!!!!!!!
!
!....
      double precision xnfc(maxd,6), xnffe(maxd,5), xnfmg(maxd,6), 
     &                 xnfn(maxd,6), xnfne(maxd,6), xnfo(maxd,6), 
     &                 xnfs(maxd,6), xnfsi(maxd,6)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   X N F B L K !!!!!!!!!!!!!!!!!
!
!....
      double precision xnfpal(maxd, 2), xnfpb(maxd, 1),  xnfpc(maxd, 4),  
     &                 xnfpca(maxd, 2), xnfpch(maxd),   xnfpfe(maxd, 1), 
     &                 xnfpk(maxd, 1),  xnfpmg(maxd, 2), xnfpn(maxd, 5),  
     &                 xnfpna(maxd, 1), xnfpne(maxd, 6), xnfpo(maxd, 6),  
     &                 xnfpoh(maxd),   xnfpsi(maxd, 2)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   X N F P B L !!!!!!!!!!!!!!!!!
!
!....
      double precision  xnmol(maxd, maxmol)
!
!!!!!!!!!!!!!!!!! E N D  O F   C O M M O N   X N M O L B !!!!!!!!!!!!!!!!!

      end module commondat

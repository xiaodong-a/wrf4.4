! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Linear Algebra Data and Routines File
! 
! Generated by KPP-2.1 symbolic chemistry Kinetics PreProcessor
!       (http://www.cs.vt.edu/~asandu/Software/KPP)
! KPP is distributed under GPL, the general public licence
!       (http://www.gnu.org/copyleft/gpl.html)
! (C) 1995-1997, V. Damian & A. Sandu, CGRER, Univ. Iowa
! (C) 1997-2005, A. Sandu, Michigan Tech, Virginia Tech
!     With important contributions from:
!        M. Damian, Villanova University, USA
!        R. Sander, Max-Planck Institute for Chemistry, Mainz, Germany
! 
! File                 : racm_soa_vbs_het_LinearAlgebra.f90
! Time                 : Mon Sep 26 23:37:35 2022
! Working directory    : /home/ess/xiaodong/WORK/WRFCHEM/model/WRF4.4/chem/KPP/mechanisms/racm_soa_vbs_het
! Equation file        : racm_soa_vbs_het.kpp
! Output root filename : racm_soa_vbs_het
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE racm_soa_vbs_het_LinearAlgebra

  USE racm_soa_vbs_het_Parameters
  USE racm_soa_vbs_het_JacobianSP

  IMPLICIT NONE

CONTAINS


! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! KppSolveTR - sparse, transposed back substitution
!   Arguments :
!      JVS       - sparse Jacobian of variables
!      X         - Vector for variables
!      XX        - Vector for output variables
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE KppSolveTR ( JVS, X, XX )

! JVS - sparse Jacobian of variables
  REAL(kind=dp) :: JVS(LU_NONZERO)
! X - Vector for variables
  REAL(kind=dp) :: X(NVAR)
! XX - Vector for output variables
  REAL(kind=dp) :: XX(NVAR)

  XX(1) = X(1)/JVS(1)
  XX(2) = X(2)/JVS(4)
  XX(3) = X(3)/JVS(7)
  XX(4) = X(4)/JVS(20)
  XX(5) = X(5)/JVS(43)
  XX(6) = (X(6)-JVS(44)*XX(5))/(JVS(46))
  XX(7) = (X(7)-JVS(47)*XX(6))/(JVS(49))
  XX(8) = (X(8)-JVS(50)*XX(7))/(JVS(52))
  XX(9) = X(9)/JVS(54)
  XX(10) = (X(10)-JVS(55)*XX(9))/(JVS(57))
  XX(11) = (X(11)-JVS(58)*XX(10))/(JVS(60))
  XX(12) = (X(12)-JVS(61)*XX(11))/(JVS(63))
  XX(13) = X(13)/JVS(65)
  XX(14) = X(14)/JVS(67)
  XX(15) = (X(15)-JVS(2)*XX(1))/(JVS(69))
  XX(16) = X(16)/JVS(71)
  XX(17) = X(17)/JVS(75)
  XX(18) = X(18)/JVS(78)
  XX(19) = X(19)/JVS(81)
  XX(20) = X(20)/JVS(84)
  XX(21) = X(21)/JVS(88)
  XX(22) = X(22)/JVS(91)
  XX(23) = X(23)/JVS(94)
  XX(24) = X(24)/JVS(97)
  XX(25) = X(25)/JVS(100)
  XX(26) = X(26)/JVS(104)
  XX(27) = X(27)/JVS(108)
  XX(28) = X(28)/JVS(113)
  XX(29) = X(29)/JVS(119)
  XX(30) = X(30)/JVS(127)
  XX(31) = X(31)/JVS(133)
  XX(32) = (X(32)-JVS(66)*XX(13)-JVS(82)*XX(19))/(JVS(139))
  XX(33) = X(33)/JVS(144)
  XX(34) = X(34)/JVS(157)
  XX(35) = (X(35)-JVS(158)*XX(34))/(JVS(169))
  XX(36) = X(36)/JVS(176)
  XX(37) = X(37)/JVS(183)
  XX(38) = (X(38)-JVS(120)*XX(29))/(JVS(190))
  XX(39) = (X(39)-JVS(121)*XX(29))/(JVS(196))
  XX(40) = (X(40)-JVS(122)*XX(29))/(JVS(201))
  XX(41) = (X(41)-JVS(159)*XX(34))/(JVS(206))
  XX(42) = (X(42)-JVS(92)*XX(22)-JVS(114)*XX(28))/(JVS(211))
  XX(43) = (X(43)-JVS(8)*XX(3)-JVS(21)*XX(4))/(JVS(217))
  XX(44) = X(44)/JVS(221)
  XX(45) = X(45)/JVS(232)
  XX(46) = (X(46)-JVS(160)*XX(34)-JVS(184)*XX(37))/(JVS(238))
  XX(47) = (X(47)-JVS(145)*XX(33))/(JVS(243))
  XX(48) = X(48)/JVS(247)
  XX(49) = (X(49)-JVS(9)*XX(3)-JVS(161)*XX(34))/(JVS(254))
  XX(50) = (X(50)-JVS(162)*XX(34))/(JVS(259))
  XX(51) = (X(51)-JVS(10)*XX(3)-JVS(22)*XX(4)-JVS(146)*XX(33))/(JVS(266))
  XX(52) = (X(52)-JVS(5)*XX(2))/(JVS(276))
  XX(53) = (X(53)-JVS(11)*XX(3)-JVS(177)*XX(36)-JVS(277)*XX(52))/(JVS(297))
  XX(54) = (X(54)-JVS(23)*XX(4)-JVS(147)*XX(33)-JVS(170)*XX(35)-JVS(260)*XX(50)-JVS(278)*XX(52))/(JVS(302))
  XX(55) = (X(55)-JVS(12)*XX(3)-JVS(128)*XX(30)-JVS(148)*XX(33)-JVS(279)*XX(52))/(JVS(306))
  XX(56) = (X(56)-JVS(85)*XX(20)-JVS(105)*XX(26)-JVS(115)*XX(28)-JVS(280)*XX(52))/(JVS(312))
  XX(57) = (X(57)-JVS(134)*XX(31)-JVS(202)*XX(40)-JVS(222)*XX(44))/(JVS(324))
  XX(58) = (X(58)-JVS(13)*XX(3)-JVS(185)*XX(37)-JVS(248)*XX(48)-JVS(281)*XX(52))/(JVS(331))
  XX(59) = (X(59)-JVS(14)*XX(3)-JVS(149)*XX(33)-JVS(163)*XX(34)-JVS(178)*XX(36)-JVS(212)*XX(42)-JVS(282)*XX(52)-JVS(313)&
             &*XX(56))/(JVS(337))
  XX(60) = (X(60)-JVS(223)*XX(44)-JVS(283)*XX(52))/(JVS(343))
  XX(61) = (X(61)-JVS(24)*XX(4))/(JVS(358))
  XX(62) = (X(62)-JVS(25)*XX(4))/(JVS(367))
  XX(63) = X(63)/JVS(377)
  XX(64) = (X(64)-JVS(15)*XX(3)-JVS(284)*XX(52)-JVS(314)*XX(56))/(JVS(391))
  XX(65) = X(65)/JVS(402)
  XX(66) = X(66)/JVS(417)
  XX(67) = (X(67)-JVS(26)*XX(4)-JVS(72)*XX(16)-JVS(213)*XX(42)-JVS(285)*XX(52)-JVS(392)*XX(64))/(JVS(440))
  XX(68) = (X(68)-JVS(344)*XX(60))/(JVS(451))
  XX(69) = (X(69)-JVS(224)*XX(44)-JVS(286)*XX(52))/(JVS(471))
  XX(70) = (X(70)-JVS(27)*XX(4)-JVS(418)*XX(66))/(JVS(491))
  XX(71) = (X(71)-JVS(345)*XX(60)-JVS(472)*XX(69))/(JVS(501))
  XX(72) = (X(72)-JVS(346)*XX(60)-JVS(473)*XX(69))/(JVS(512))
  XX(73) = (X(73)-JVS(164)*XX(34)-JVS(225)*XX(44)-JVS(287)*XX(52))/(JVS(549))
  XX(74) = (X(74)-JVS(28)*XX(4)-JVS(332)*XX(58)-JVS(474)*XX(69)-JVS(550)*XX(73))/(JVS(572))
  XX(75) = (X(75)-JVS(419)*XX(66))/(JVS(590))
  XX(76) = (X(76)-JVS(29)*XX(4)-JVS(420)*XX(66)-JVS(551)*XX(73)-JVS(591)*XX(75))/(JVS(607))
  XX(77) = (X(77)-JVS(30)*XX(4)-JVS(421)*XX(66)-JVS(552)*XX(73)-JVS(592)*XX(75))/(JVS(618))
  XX(78) = (X(78)-JVS(165)*XX(34)-JVS(226)*XX(44)-JVS(288)*XX(52))/(JVS(643))
  XX(79) = (X(79)-JVS(16)*XX(3)-JVS(31)*XX(4)-JVS(89)*XX(21)-JVS(227)*XX(44)-JVS(233)*XX(45)-JVS(289)*XX(52)-JVS(347)&
             &*XX(60)-JVS(422)*XX(66)-JVS(475)*XX(69)-JVS(553)*XX(73)-JVS(573)*XX(74)-JVS(593)*XX(75)-JVS(644)*XX(78))&
             &/(JVS(664))
  XX(80) = (X(80)-JVS(554)*XX(73))/(JVS(692))
  XX(81) = (X(81)-JVS(32)*XX(4)-JVS(150)*XX(33)-JVS(166)*XX(34)-JVS(171)*XX(35)-JVS(179)*XX(36)-JVS(207)*XX(41)-JVS(239)&
             &*XX(46)-JVS(255)*XX(49)-JVS(261)*XX(50)-JVS(290)*XX(52)-JVS(348)*XX(60)-JVS(423)*XX(66)-JVS(476)*XX(69)&
             &-JVS(492)*XX(70)-JVS(555)*XX(73)-JVS(608)*XX(76)-JVS(619)*XX(77)-JVS(645)*XX(78)-JVS(693)*XX(80))/(JVS(709))
  XX(82) = (X(82)-JVS(17)*XX(3)-JVS(33)*XX(4)-JVS(151)*XX(33)-JVS(172)*XX(35)-JVS(180)*XX(36)-JVS(208)*XX(41)-JVS(240)&
             &*XX(46)-JVS(256)*XX(49)-JVS(262)*XX(50)-JVS(291)*XX(52)-JVS(349)*XX(60)-JVS(359)*XX(61)-JVS(424)*XX(66)&
             &-JVS(477)*XX(69)-JVS(556)*XX(73)-JVS(609)*XX(76)-JVS(620)*XX(77)-JVS(646)*XX(78)-JVS(694)*XX(80))/(JVS(722))
  XX(83) = (X(83)-JVS(34)*XX(4)-JVS(425)*XX(66)-JVS(557)*XX(73)-JVS(594)*XX(75)-JVS(647)*XX(78))/(JVS(739))
  XX(84) = (X(84)-JVS(35)*XX(4)-JVS(426)*XX(66)-JVS(558)*XX(73)-JVS(595)*XX(75)-JVS(648)*XX(78)-JVS(740)*XX(83))&
             &/(JVS(758))
  XX(85) = (X(85)-JVS(18)*XX(3)-JVS(36)*XX(4)-JVS(68)*XX(14)-JVS(129)*XX(30)-JVS(140)*XX(32)-JVS(152)*XX(33)-JVS(173)&
             &*XX(35)-JVS(191)*XX(38)-JVS(197)*XX(39)-JVS(203)*XX(40)-JVS(218)*XX(43)-JVS(234)*XX(45)-JVS(244)*XX(47)&
             &-JVS(249)*XX(48)-JVS(263)*XX(50)-JVS(267)*XX(51)-JVS(292)*XX(52)-JVS(298)*XX(53)-JVS(303)*XX(54)-JVS(307)&
             &*XX(55)-JVS(315)*XX(56)-JVS(325)*XX(57)-JVS(333)*XX(58)-JVS(338)*XX(59)-JVS(350)*XX(60)-JVS(368)*XX(62)&
             &-JVS(378)*XX(63)-JVS(393)*XX(64)-JVS(403)*XX(65)-JVS(427)*XX(66)-JVS(441)*XX(67)-JVS(452)*XX(68)-JVS(478)&
             &*XX(69)-JVS(493)*XX(70)-JVS(502)*XX(71)-JVS(513)*XX(72)-JVS(559)*XX(73)-JVS(574)*XX(74)-JVS(596)*XX(75)&
             &-JVS(649)*XX(78)-JVS(665)*XX(79)-JVS(695)*XX(80)-JVS(710)*XX(81)-JVS(723)*XX(82)-JVS(741)*XX(83)-JVS(759)&
             &*XX(84))/(JVS(786))
  XX(86) = (X(86)-JVS(37)*XX(4)-JVS(351)*XX(60)-JVS(428)*XX(66)-JVS(479)*XX(69)-JVS(560)*XX(73)-JVS(597)*XX(75)-JVS(650)&
             &*XX(78)-JVS(696)*XX(80))/(JVS(805))
  XX(87) = (X(87)-JVS(38)*XX(4)-JVS(186)*XX(37)-JVS(429)*XX(66)-JVS(480)*XX(69)-JVS(561)*XX(73)-JVS(651)*XX(78)-JVS(697)&
             &*XX(80))/(JVS(831))
  XX(88) = (X(88)-JVS(79)*XX(18)-JVS(95)*XX(23)-JVS(98)*XX(24)-JVS(141)*XX(32)-JVS(167)*XX(34)-JVS(174)*XX(35)-JVS(181)&
             &*XX(36)-JVS(192)*XX(38)-JVS(198)*XX(39)-JVS(209)*XX(41)-JVS(241)*XX(46)-JVS(257)*XX(49)-JVS(264)*XX(50)&
             &-JVS(293)*XX(52)-JVS(299)*XX(53)-JVS(326)*XX(57)-JVS(339)*XX(59)-JVS(352)*XX(60)-JVS(369)*XX(62)-JVS(394)&
             &*XX(64)-JVS(430)*XX(66)-JVS(442)*XX(67)-JVS(453)*XX(68)-JVS(481)*XX(69)-JVS(503)*XX(71)-JVS(514)*XX(72)&
             &-JVS(562)*XX(73)-JVS(598)*XX(75)-JVS(610)*XX(76)-JVS(621)*XX(77)-JVS(652)*XX(78)-JVS(666)*XX(79)-JVS(698)&
             &*XX(80)-JVS(711)*XX(81)-JVS(724)*XX(82)-JVS(742)*XX(83)-JVS(760)*XX(84)-JVS(787)*XX(85)-JVS(806)*XX(86)&
             &-JVS(832)*XX(87))/(JVS(871))
  XX(89) = (X(89)-JVS(39)*XX(4)-JVS(73)*XX(16)-JVS(83)*XX(19)-JVS(86)*XX(20)-JVS(101)*XX(25)-JVS(109)*XX(27)-JVS(135)&
             &*XX(31)-JVS(142)*XX(32)-JVS(153)*XX(33)-JVS(228)*XX(44)-JVS(235)*XX(45)-JVS(316)*XX(56)-JVS(327)*XX(57)&
             &-JVS(360)*XX(61)-JVS(370)*XX(62)-JVS(379)*XX(63)-JVS(395)*XX(64)-JVS(404)*XX(65)-JVS(431)*XX(66)-JVS(443)&
             &*XX(67)-JVS(454)*XX(68)-JVS(482)*XX(69)-JVS(494)*XX(70)-JVS(504)*XX(71)-JVS(515)*XX(72)-JVS(563)*XX(73)&
             &-JVS(575)*XX(74)-JVS(599)*XX(75)-JVS(611)*XX(76)-JVS(622)*XX(77)-JVS(653)*XX(78)-JVS(667)*XX(79)-JVS(699)&
             &*XX(80)-JVS(712)*XX(81)-JVS(725)*XX(82)-JVS(743)*XX(83)-JVS(761)*XX(84)-JVS(788)*XX(85)-JVS(807)*XX(86)&
             &-JVS(833)*XX(87)-JVS(872)*XX(88))/(JVS(940))
  XX(90) = (X(90)-JVS(40)*XX(4)-JVS(102)*XX(25)-JVS(187)*XX(37)-JVS(353)*XX(60)-JVS(361)*XX(61)-JVS(371)*XX(62)-JVS(380)&
             &*XX(63)-JVS(396)*XX(64)-JVS(405)*XX(65)-JVS(432)*XX(66)-JVS(444)*XX(67)-JVS(455)*XX(68)-JVS(483)*XX(69)&
             &-JVS(495)*XX(70)-JVS(505)*XX(71)-JVS(516)*XX(72)-JVS(564)*XX(73)-JVS(576)*XX(74)-JVS(600)*XX(75)-JVS(612)&
             &*XX(76)-JVS(623)*XX(77)-JVS(654)*XX(78)-JVS(668)*XX(79)-JVS(700)*XX(80)-JVS(713)*XX(81)-JVS(726)*XX(82)&
             &-JVS(744)*XX(83)-JVS(762)*XX(84)-JVS(789)*XX(85)-JVS(808)*XX(86)-JVS(834)*XX(87)-JVS(873)*XX(88)-JVS(941)&
             &*XX(89))/(JVS(979))
  XX(91) = (X(91)-JVS(3)*XX(1)-JVS(6)*XX(2)-JVS(19)*XX(3)-JVS(45)*XX(5)-JVS(48)*XX(6)-JVS(51)*XX(7)-JVS(53)*XX(8)&
             &-JVS(56)*XX(9)-JVS(59)*XX(10)-JVS(62)*XX(11)-JVS(64)*XX(12)-JVS(70)*XX(15)-JVS(74)*XX(16)-JVS(87)*XX(20)&
             &-JVS(90)*XX(21)-JVS(93)*XX(22)-JVS(96)*XX(23)-JVS(99)*XX(24)-JVS(103)*XX(25)-JVS(106)*XX(26)-JVS(110)*XX(27)&
             &-JVS(116)*XX(28)-JVS(123)*XX(29)-JVS(136)*XX(31)-JVS(154)*XX(33)-JVS(168)*XX(34)-JVS(175)*XX(35)-JVS(182)&
             &*XX(36)-JVS(188)*XX(37)-JVS(193)*XX(38)-JVS(199)*XX(39)-JVS(204)*XX(40)-JVS(210)*XX(41)-JVS(214)*XX(42)&
             &-JVS(219)*XX(43)-JVS(229)*XX(44)-JVS(236)*XX(45)-JVS(242)*XX(46)-JVS(245)*XX(47)-JVS(250)*XX(48)-JVS(258)&
             &*XX(49)-JVS(265)*XX(50)-JVS(268)*XX(51)-JVS(294)*XX(52)-JVS(300)*XX(53)-JVS(304)*XX(54)-JVS(308)*XX(55)&
             &-JVS(317)*XX(56)-JVS(328)*XX(57)-JVS(334)*XX(58)-JVS(340)*XX(59)-JVS(354)*XX(60)-JVS(362)*XX(61)-JVS(372)&
             &*XX(62)-JVS(381)*XX(63)-JVS(397)*XX(64)-JVS(406)*XX(65)-JVS(433)*XX(66)-JVS(445)*XX(67)-JVS(456)*XX(68)&
             &-JVS(484)*XX(69)-JVS(496)*XX(70)-JVS(506)*XX(71)-JVS(517)*XX(72)-JVS(565)*XX(73)-JVS(577)*XX(74)-JVS(601)&
             &*XX(75)-JVS(613)*XX(76)-JVS(624)*XX(77)-JVS(655)*XX(78)-JVS(669)*XX(79)-JVS(701)*XX(80)-JVS(714)*XX(81)&
             &-JVS(727)*XX(82)-JVS(745)*XX(83)-JVS(763)*XX(84)-JVS(790)*XX(85)-JVS(809)*XX(86)-JVS(835)*XX(87)-JVS(874)&
             &*XX(88)-JVS(942)*XX(89)-JVS(980)*XX(90))/(JVS(1055))
  XX(92) = (X(92)-JVS(434)*XX(66)-JVS(656)*XX(78)-JVS(702)*XX(80)-JVS(810)*XX(86)-JVS(836)*XX(87)-JVS(875)*XX(88)&
             &-JVS(943)*XX(89)-JVS(981)*XX(90)-JVS(1056)*XX(91))/(JVS(1084))
  XX(93) = (X(93)-JVS(41)*XX(4)-JVS(237)*XX(45)-JVS(251)*XX(48)-JVS(355)*XX(60)-JVS(363)*XX(61)-JVS(373)*XX(62)-JVS(382)&
             &*XX(63)-JVS(398)*XX(64)-JVS(407)*XX(65)-JVS(435)*XX(66)-JVS(446)*XX(67)-JVS(457)*XX(68)-JVS(485)*XX(69)&
             &-JVS(497)*XX(70)-JVS(507)*XX(71)-JVS(518)*XX(72)-JVS(566)*XX(73)-JVS(578)*XX(74)-JVS(602)*XX(75)-JVS(614)&
             &*XX(76)-JVS(625)*XX(77)-JVS(657)*XX(78)-JVS(670)*XX(79)-JVS(703)*XX(80)-JVS(715)*XX(81)-JVS(728)*XX(82)&
             &-JVS(746)*XX(83)-JVS(764)*XX(84)-JVS(791)*XX(85)-JVS(811)*XX(86)-JVS(837)*XX(87)-JVS(876)*XX(88)-JVS(944)&
             &*XX(89)-JVS(982)*XX(90)-JVS(1057)*XX(91)-JVS(1085)*XX(92))/(JVS(1129))
  XX(94) = (X(94)-JVS(117)*XX(28)-JVS(124)*XX(29)-JVS(130)*XX(30)-JVS(143)*XX(32)-JVS(215)*XX(42)-JVS(295)*XX(52)&
             &-JVS(318)*XX(56)-JVS(356)*XX(60)-JVS(364)*XX(61)-JVS(374)*XX(62)-JVS(383)*XX(63)-JVS(399)*XX(64)-JVS(408)&
             &*XX(65)-JVS(436)*XX(66)-JVS(447)*XX(67)-JVS(458)*XX(68)-JVS(486)*XX(69)-JVS(498)*XX(70)-JVS(508)*XX(71)&
             &-JVS(519)*XX(72)-JVS(567)*XX(73)-JVS(579)*XX(74)-JVS(603)*XX(75)-JVS(615)*XX(76)-JVS(626)*XX(77)-JVS(658)&
             &*XX(78)-JVS(671)*XX(79)-JVS(704)*XX(80)-JVS(716)*XX(81)-JVS(729)*XX(82)-JVS(747)*XX(83)-JVS(765)*XX(84)&
             &-JVS(792)*XX(85)-JVS(812)*XX(86)-JVS(838)*XX(87)-JVS(877)*XX(88)-JVS(945)*XX(89)-JVS(983)*XX(90)-JVS(1058)&
             &*XX(91)-JVS(1086)*XX(92)-JVS(1130)*XX(93))/(JVS(1172))
  XX(95) = (X(95)-JVS(76)*XX(17)-JVS(80)*XX(18)-JVS(107)*XX(26)-JVS(111)*XX(27)-JVS(118)*XX(28)-JVS(125)*XX(29)-JVS(131)&
             &*XX(30)-JVS(137)*XX(31)-JVS(194)*XX(38)-JVS(200)*XX(39)-JVS(205)*XX(40)-JVS(230)*XX(44)-JVS(252)*XX(48)&
             &-JVS(319)*XX(56)-JVS(329)*XX(57)-JVS(335)*XX(58)-JVS(459)*XX(68)-JVS(487)*XX(69)-JVS(509)*XX(71)-JVS(520)&
             &*XX(72)-JVS(568)*XX(73)-JVS(580)*XX(74)-JVS(604)*XX(75)-JVS(659)*XX(78)-JVS(672)*XX(79)-JVS(705)*XX(80)&
             &-JVS(793)*XX(85)-JVS(813)*XX(86)-JVS(839)*XX(87)-JVS(878)*XX(88)-JVS(946)*XX(89)-JVS(984)*XX(90)-JVS(1059)&
             &*XX(91)-JVS(1087)*XX(92)-JVS(1131)*XX(93)-JVS(1173)*XX(94))/(JVS(1230))
  XX(96) = (X(96)-JVS(77)*XX(17)-JVS(132)*XX(30)-JVS(138)*XX(31)-JVS(216)*XX(42)-JVS(220)*XX(43)-JVS(231)*XX(44)&
             &-JVS(246)*XX(47)-JVS(253)*XX(48)-JVS(269)*XX(51)-JVS(296)*XX(52)-JVS(301)*XX(53)-JVS(305)*XX(54)-JVS(309)&
             &*XX(55)-JVS(330)*XX(57)-JVS(336)*XX(58)-JVS(341)*XX(59)-JVS(357)*XX(60)-JVS(365)*XX(61)-JVS(375)*XX(62)&
             &-JVS(384)*XX(63)-JVS(400)*XX(64)-JVS(409)*XX(65)-JVS(437)*XX(66)-JVS(448)*XX(67)-JVS(460)*XX(68)-JVS(488)&
             &*XX(69)-JVS(499)*XX(70)-JVS(510)*XX(71)-JVS(521)*XX(72)-JVS(569)*XX(73)-JVS(581)*XX(74)-JVS(605)*XX(75)&
             &-JVS(616)*XX(76)-JVS(627)*XX(77)-JVS(660)*XX(78)-JVS(673)*XX(79)-JVS(706)*XX(80)-JVS(717)*XX(81)-JVS(730)&
             &*XX(82)-JVS(748)*XX(83)-JVS(766)*XX(84)-JVS(794)*XX(85)-JVS(814)*XX(86)-JVS(840)*XX(87)-JVS(879)*XX(88)&
             &-JVS(947)*XX(89)-JVS(985)*XX(90)-JVS(1060)*XX(91)-JVS(1088)*XX(92)-JVS(1132)*XX(93)-JVS(1174)*XX(94)-JVS(1231)&
             &*XX(95))/(JVS(1279))
  XX(97) = (X(97)-JVS(42)*XX(4)-JVS(570)*XX(73)-JVS(661)*XX(78)-JVS(880)*XX(88)-JVS(948)*XX(89)-JVS(986)*XX(90)&
             &-JVS(1061)*XX(91)-JVS(1089)*XX(92)-JVS(1133)*XX(93)-JVS(1175)*XX(94)-JVS(1232)*XX(95)-JVS(1280)*XX(96))&
             &/(JVS(1307))
  XX(97) = XX(97)
  XX(96) = XX(96)-JVS(1306)*XX(97)
  XX(95) = XX(95)-JVS(1278)*XX(96)-JVS(1305)*XX(97)
  XX(94) = XX(94)-JVS(1229)*XX(95)-JVS(1277)*XX(96)-JVS(1304)*XX(97)
  XX(93) = XX(93)-JVS(1171)*XX(94)-JVS(1228)*XX(95)-JVS(1276)*XX(96)-JVS(1303)*XX(97)
  XX(92) = XX(92)-JVS(1128)*XX(93)-JVS(1170)*XX(94)-JVS(1227)*XX(95)-JVS(1275)*XX(96)-JVS(1302)*XX(97)
  XX(91) = XX(91)-JVS(1083)*XX(92)-JVS(1127)*XX(93)-JVS(1169)*XX(94)-JVS(1226)*XX(95)-JVS(1274)*XX(96)-JVS(1301)*XX(97)
  XX(90) = XX(90)-JVS(1054)*XX(91)-JVS(1082)*XX(92)-JVS(1126)*XX(93)-JVS(1168)*XX(94)-JVS(1225)*XX(95)-JVS(1273)*XX(96)&
             &-JVS(1300)*XX(97)
  XX(89) = XX(89)-JVS(978)*XX(90)-JVS(1053)*XX(91)-JVS(1081)*XX(92)-JVS(1125)*XX(93)-JVS(1167)*XX(94)-JVS(1224)*XX(95)&
             &-JVS(1272)*XX(96)-JVS(1299)*XX(97)
  XX(88) = XX(88)-JVS(939)*XX(89)-JVS(977)*XX(90)-JVS(1052)*XX(91)-JVS(1080)*XX(92)-JVS(1124)*XX(93)-JVS(1166)*XX(94)&
             &-JVS(1223)*XX(95)-JVS(1271)*XX(96)-JVS(1298)*XX(97)
  XX(87) = XX(87)-JVS(870)*XX(88)-JVS(938)*XX(89)-JVS(976)*XX(90)-JVS(1051)*XX(91)-JVS(1079)*XX(92)-JVS(1123)*XX(93)&
             &-JVS(1165)*XX(94)-JVS(1222)*XX(95)-JVS(1270)*XX(96)-JVS(1297)*XX(97)
  XX(86) = XX(86)-JVS(830)*XX(87)-JVS(869)*XX(88)-JVS(937)*XX(89)-JVS(975)*XX(90)-JVS(1050)*XX(91)-JVS(1078)*XX(92)&
             &-JVS(1122)*XX(93)-JVS(1164)*XX(94)-JVS(1221)*XX(95)-JVS(1269)*XX(96)-JVS(1296)*XX(97)
  XX(85) = XX(85)-JVS(804)*XX(86)-JVS(829)*XX(87)-JVS(868)*XX(88)-JVS(936)*XX(89)-JVS(974)*XX(90)-JVS(1049)*XX(91)&
             &-JVS(1077)*XX(92)-JVS(1121)*XX(93)-JVS(1163)*XX(94)-JVS(1220)*XX(95)-JVS(1268)*XX(96)-JVS(1295)*XX(97)
  XX(84) = XX(84)-JVS(803)*XX(86)-JVS(828)*XX(87)-JVS(867)*XX(88)-JVS(935)*XX(89)-JVS(973)*XX(90)-JVS(1048)*XX(91)&
             &-JVS(1120)*XX(93)-JVS(1162)*XX(94)-JVS(1219)*XX(95)-JVS(1267)*XX(96)-JVS(1294)*XX(97)
  XX(83) = XX(83)-JVS(757)*XX(84)-JVS(802)*XX(86)-JVS(827)*XX(87)-JVS(866)*XX(88)-JVS(934)*XX(89)-JVS(972)*XX(90)&
             &-JVS(1047)*XX(91)-JVS(1119)*XX(93)-JVS(1161)*XX(94)-JVS(1218)*XX(95)-JVS(1266)*XX(96)-JVS(1293)*XX(97)
  XX(82) = XX(82)-JVS(738)*XX(83)-JVS(756)*XX(84)-JVS(785)*XX(85)-JVS(801)*XX(86)-JVS(826)*XX(87)-JVS(865)*XX(88)&
             &-JVS(933)*XX(89)-JVS(971)*XX(90)-JVS(1046)*XX(91)-JVS(1076)*XX(92)-JVS(1118)*XX(93)-JVS(1160)*XX(94)-JVS(1217)&
             &*XX(95)-JVS(1265)*XX(96)-JVS(1292)*XX(97)
  XX(81) = XX(81)-JVS(721)*XX(82)-JVS(737)*XX(83)-JVS(755)*XX(84)-JVS(784)*XX(85)-JVS(800)*XX(86)-JVS(825)*XX(87)&
             &-JVS(864)*XX(88)-JVS(932)*XX(89)-JVS(970)*XX(90)-JVS(1045)*XX(91)-JVS(1075)*XX(92)-JVS(1117)*XX(93)-JVS(1159)&
             &*XX(94)-JVS(1216)*XX(95)-JVS(1264)*XX(96)-JVS(1291)*XX(97)
  XX(80) = XX(80)-JVS(863)*XX(88)-JVS(931)*XX(89)-JVS(969)*XX(90)-JVS(1044)*XX(91)-JVS(1074)*XX(92)-JVS(1116)*XX(93)&
             &-JVS(1158)*XX(94)-JVS(1215)*XX(95)-JVS(1263)*XX(96)
  XX(79) = XX(79)-JVS(691)*XX(80)-JVS(783)*XX(85)-JVS(799)*XX(86)-JVS(824)*XX(87)-JVS(862)*XX(88)-JVS(930)*XX(89)&
             &-JVS(968)*XX(90)-JVS(1043)*XX(91)-JVS(1073)*XX(92)-JVS(1115)*XX(93)-JVS(1157)*XX(94)-JVS(1214)*XX(95)&
             &-JVS(1262)*XX(96)-JVS(1290)*XX(97)
  XX(78) = XX(78)-JVS(861)*XX(88)-JVS(929)*XX(89)-JVS(967)*XX(90)-JVS(1042)*XX(91)-JVS(1114)*XX(93)-JVS(1213)*XX(95)&
             &-JVS(1261)*XX(96)
  XX(77) = XX(77)-JVS(642)*XX(78)-JVS(690)*XX(80)-JVS(798)*XX(86)-JVS(823)*XX(87)-JVS(860)*XX(88)-JVS(928)*XX(89)&
             &-JVS(966)*XX(90)-JVS(1041)*XX(91)-JVS(1072)*XX(92)-JVS(1113)*XX(93)-JVS(1156)*XX(94)-JVS(1212)*XX(95)&
             &-JVS(1260)*XX(96)-JVS(1289)*XX(97)
  XX(76) = XX(76)-JVS(641)*XX(78)-JVS(689)*XX(80)-JVS(797)*XX(86)-JVS(822)*XX(87)-JVS(859)*XX(88)-JVS(927)*XX(89)&
             &-JVS(965)*XX(90)-JVS(1040)*XX(91)-JVS(1071)*XX(92)-JVS(1112)*XX(93)-JVS(1155)*XX(94)-JVS(1211)*XX(95)&
             &-JVS(1259)*XX(96)-JVS(1288)*XX(97)
  XX(75) = XX(75)-JVS(640)*XX(78)-JVS(796)*XX(86)-JVS(821)*XX(87)-JVS(926)*XX(89)-JVS(1039)*XX(91)-JVS(1111)*XX(93)&
             &-JVS(1210)*XX(95)-JVS(1287)*XX(97)
  XX(74) = XX(74)-JVS(589)*XX(75)-JVS(688)*XX(80)-JVS(782)*XX(85)-JVS(858)*XX(88)-JVS(925)*XX(89)-JVS(964)*XX(90)&
             &-JVS(1038)*XX(91)-JVS(1070)*XX(92)-JVS(1110)*XX(93)-JVS(1154)*XX(94)-JVS(1209)*XX(95)-JVS(1258)*XX(96)
  XX(73) = XX(73)-JVS(857)*XX(88)-JVS(924)*XX(89)-JVS(1037)*XX(91)-JVS(1208)*XX(95)-JVS(1257)*XX(96)
  XX(72) = XX(72)-JVS(548)*XX(73)-JVS(588)*XX(75)-JVS(663)*XX(79)-JVS(923)*XX(89)-JVS(963)*XX(90)-JVS(1036)*XX(91)&
             &-JVS(1069)*XX(92)-JVS(1109)*XX(93)-JVS(1153)*XX(94)-JVS(1207)*XX(95)-JVS(1256)*XX(96)
  XX(71) = XX(71)-JVS(547)*XX(73)-JVS(587)*XX(75)-JVS(662)*XX(79)-JVS(922)*XX(89)-JVS(962)*XX(90)-JVS(1035)*XX(91)&
             &-JVS(1068)*XX(92)-JVS(1108)*XX(93)-JVS(1152)*XX(94)-JVS(1206)*XX(95)-JVS(1255)*XX(96)
  XX(70) = XX(70)-JVS(546)*XX(73)-JVS(639)*XX(78)-JVS(820)*XX(87)-JVS(921)*XX(89)-JVS(961)*XX(90)-JVS(1034)*XX(91)&
             &-JVS(1067)*XX(92)-JVS(1107)*XX(93)-JVS(1151)*XX(94)-JVS(1205)*XX(95)-JVS(1254)*XX(96)-JVS(1286)*XX(97)
  XX(69) = XX(69)-JVS(920)*XX(89)-JVS(1033)*XX(91)-JVS(1106)*XX(93)-JVS(1204)*XX(95)-JVS(1253)*XX(96)
  XX(68) = XX(68)-JVS(470)*XX(69)-JVS(545)*XX(73)-JVS(919)*XX(89)-JVS(960)*XX(90)-JVS(1032)*XX(91)-JVS(1066)*XX(92)&
             &-JVS(1105)*XX(93)-JVS(1150)*XX(94)-JVS(1203)*XX(95)-JVS(1252)*XX(96)
  XX(67) = XX(67)-JVS(469)*XX(69)-JVS(544)*XX(73)-JVS(708)*XX(81)-JVS(720)*XX(82)-JVS(781)*XX(85)-JVS(918)*XX(89)&
             &-JVS(959)*XX(90)-JVS(1031)*XX(91)-JVS(1104)*XX(93)-JVS(1149)*XX(94)-JVS(1202)*XX(95)
  XX(66) = XX(66)-JVS(819)*XX(87)-JVS(1030)*XX(91)-JVS(1103)*XX(93)-JVS(1285)*XX(97)
  XX(65) = XX(65)-JVS(416)*XX(66)-JVS(543)*XX(73)-JVS(586)*XX(75)-JVS(638)*XX(78)-JVS(917)*XX(89)-JVS(958)*XX(90)&
             &-JVS(1065)*XX(92)-JVS(1102)*XX(93)-JVS(1148)*XX(94)-JVS(1201)*XX(95)-JVS(1251)*XX(96)
  XX(64) = XX(64)-JVS(468)*XX(69)-JVS(542)*XX(73)-JVS(780)*XX(85)-JVS(916)*XX(89)-JVS(1029)*XX(91)-JVS(1101)*XX(93)&
             &-JVS(1147)*XX(94)-JVS(1200)*XX(95)
  XX(63) = XX(63)-JVS(390)*XX(64)-JVS(541)*XX(73)-JVS(585)*XX(75)-JVS(707)*XX(81)-JVS(915)*XX(89)-JVS(957)*XX(90)&
             &-JVS(1064)*XX(92)-JVS(1100)*XX(93)-JVS(1146)*XX(94)-JVS(1199)*XX(95)-JVS(1250)*XX(96)
  XX(62) = XX(62)-JVS(540)*XX(73)-JVS(637)*XX(78)-JVS(914)*XX(89)-JVS(956)*XX(90)-JVS(1063)*XX(92)-JVS(1099)*XX(93)&
             &-JVS(1145)*XX(94)-JVS(1198)*XX(95)-JVS(1249)*XX(96)
  XX(61) = XX(61)-JVS(415)*XX(66)-JVS(539)*XX(73)-JVS(636)*XX(78)-JVS(913)*XX(89)-JVS(955)*XX(90)-JVS(1062)*XX(92)&
             &-JVS(1098)*XX(93)-JVS(1144)*XX(94)-JVS(1197)*XX(95)-JVS(1248)*XX(96)
  XX(60) = XX(60)-JVS(538)*XX(73)-JVS(912)*XX(89)-JVS(1028)*XX(91)-JVS(1196)*XX(95)-JVS(1247)*XX(96)
  XX(59) = XX(59)-JVS(389)*XX(64)-JVS(439)*XX(67)-JVS(467)*XX(69)-JVS(537)*XX(73)-JVS(687)*XX(80)-JVS(779)*XX(85)&
             &-JVS(856)*XX(88)-JVS(911)*XX(89)-JVS(954)*XX(90)-JVS(1027)*XX(91)-JVS(1097)*XX(93)-JVS(1143)*XX(94)-JVS(1195)&
             &*XX(95)-JVS(1246)*XX(96)
  XX(58) = XX(58)-JVS(466)*XX(69)-JVS(536)*XX(73)-JVS(571)*XX(74)-JVS(584)*XX(75)-JVS(686)*XX(80)-JVS(778)*XX(85)&
             &-JVS(910)*XX(89)-JVS(1026)*XX(91)-JVS(1096)*XX(93)-JVS(1194)*XX(95)-JVS(1245)*XX(96)
  XX(57) = XX(57)-JVS(450)*XX(68)-JVS(583)*XX(75)-JVS(685)*XX(80)-JVS(777)*XX(85)-JVS(909)*XX(89)-JVS(1025)*XX(91)&
             &-JVS(1142)*XX(94)-JVS(1193)*XX(95)-JVS(1244)*XX(96)
  XX(56) = XX(56)-JVS(465)*XX(69)-JVS(535)*XX(73)-JVS(908)*XX(89)-JVS(1024)*XX(91)-JVS(1095)*XX(93)-JVS(1141)*XX(94)&
             &-JVS(1192)*XX(95)
  XX(55) = XX(55)-JVS(388)*XX(64)-JVS(438)*XX(67)-JVS(534)*XX(73)-JVS(684)*XX(80)-JVS(719)*XX(82)-JVS(736)*XX(83)&
             &-JVS(754)*XX(84)-JVS(776)*XX(85)-JVS(818)*XX(87)-JVS(907)*XX(89)-JVS(953)*XX(90)-JVS(1023)*XX(91)-JVS(1094)&
             &*XX(93)-JVS(1140)*XX(94)-JVS(1191)*XX(95)-JVS(1243)*XX(96)
  XX(54) = XX(54)-JVS(414)*XX(66)-JVS(490)*XX(70)-JVS(533)*XX(73)-JVS(635)*XX(78)-JVS(683)*XX(80)-JVS(735)*XX(83)&
             &-JVS(753)*XX(84)-JVS(775)*XX(85)-JVS(817)*XX(87)-JVS(855)*XX(88)-JVS(906)*XX(89)-JVS(952)*XX(90)-JVS(1022)&
             &*XX(91)-JVS(1242)*XX(96)-JVS(1284)*XX(97)
  XX(53) = XX(53)-JVS(366)*XX(62)-JVS(532)*XX(73)-JVS(682)*XX(80)-JVS(734)*XX(83)-JVS(752)*XX(84)-JVS(774)*XX(85)&
             &-JVS(854)*XX(88)-JVS(905)*XX(89)-JVS(1021)*XX(91)-JVS(1241)*XX(96)
  XX(52) = XX(52)-JVS(904)*XX(89)-JVS(1020)*XX(91)
  XX(51) = XX(51)-JVS(275)*XX(52)-JVS(376)*XX(63)-JVS(387)*XX(64)-JVS(531)*XX(73)-JVS(718)*XX(82)-JVS(733)*XX(83)&
             &-JVS(751)*XX(84)-JVS(773)*XX(85)-JVS(816)*XX(87)-JVS(903)*XX(89)-JVS(1019)*XX(91)-JVS(1240)*XX(96)-JVS(1283)&
             &*XX(97)
  XX(50) = XX(50)-JVS(634)*XX(78)-JVS(681)*XX(80)-JVS(853)*XX(88)-JVS(902)*XX(89)-JVS(1018)*XX(91)-JVS(1282)*XX(97)
  XX(49) = XX(49)-JVS(274)*XX(52)-JVS(342)*XX(60)-JVS(530)*XX(73)-JVS(633)*XX(78)-JVS(680)*XX(80)-JVS(795)*XX(86)&
             &-JVS(852)*XX(88)-JVS(901)*XX(89)-JVS(1017)*XX(91)
  XX(48) = XX(48)-JVS(529)*XX(73)-JVS(679)*XX(80)-JVS(1016)*XX(91)-JVS(1093)*XX(93)-JVS(1190)*XX(95)-JVS(1239)*XX(96)
  XX(47) = XX(47)-JVS(273)*XX(52)-JVS(401)*XX(65)-JVS(413)*XX(66)-JVS(632)*XX(78)-JVS(732)*XX(83)-JVS(750)*XX(84)&
             &-JVS(772)*XX(85)-JVS(815)*XX(87)-JVS(900)*XX(89)-JVS(1015)*XX(91)-JVS(1238)*XX(96)-JVS(1281)*XX(97)
  XX(46) = XX(46)-JVS(464)*XX(69)-JVS(528)*XX(73)-JVS(617)*XX(77)-JVS(631)*XX(78)-JVS(678)*XX(80)-JVS(851)*XX(88)&
             &-JVS(899)*XX(89)-JVS(1014)*XX(91)-JVS(1092)*XX(93)
  XX(45) = XX(45)-JVS(527)*XX(73)-JVS(677)*XX(80)-JVS(898)*XX(89)-JVS(951)*XX(90)-JVS(1013)*XX(91)-JVS(1091)*XX(93)
  XX(44) = XX(44)-JVS(1012)*XX(91)-JVS(1189)*XX(95)-JVS(1237)*XX(96)
  XX(43) = XX(43)-JVS(386)*XX(64)-JVS(412)*XX(66)-JVS(489)*XX(70)-JVS(526)*XX(73)-JVS(630)*XX(78)-JVS(731)*XX(83)&
             &-JVS(749)*XX(84)-JVS(771)*XX(85)-JVS(1011)*XX(91)-JVS(1236)*XX(96)
  XX(42) = XX(42)-JVS(272)*XX(52)-JVS(463)*XX(69)-JVS(525)*XX(73)-JVS(897)*XX(89)-JVS(1010)*XX(91)-JVS(1188)*XX(95)
  XX(41) = XX(41)-JVS(411)*XX(66)-JVS(606)*XX(76)-JVS(629)*XX(78)-JVS(676)*XX(80)-JVS(850)*XX(88)-JVS(896)*XX(89)&
             &-JVS(1009)*XX(91)
  XX(40) = XX(40)-JVS(323)*XX(57)-JVS(449)*XX(68)-JVS(770)*XX(85)-JVS(895)*XX(89)-JVS(1008)*XX(91)-JVS(1139)*XX(94)&
             &-JVS(1187)*XX(95)
  XX(39) = XX(39)-JVS(322)*XX(57)-JVS(511)*XX(72)-JVS(769)*XX(85)-JVS(894)*XX(89)-JVS(1007)*XX(91)-JVS(1138)*XX(94)&
             &-JVS(1186)*XX(95)
  XX(38) = XX(38)-JVS(321)*XX(57)-JVS(500)*XX(71)-JVS(768)*XX(85)-JVS(893)*XX(89)-JVS(1006)*XX(91)-JVS(1137)*XX(94)&
             &-JVS(1185)*XX(95)
  XX(37) = XX(37)-JVS(462)*XX(69)-JVS(524)*XX(73)-JVS(892)*XX(89)-JVS(1005)*XX(91)-JVS(1090)*XX(93)
  XX(36) = XX(36)-JVS(271)*XX(52)-JVS(849)*XX(88)-JVS(891)*XX(89)-JVS(1004)*XX(91)
  XX(35) = XX(35)-JVS(848)*XX(88)-JVS(950)*XX(90)-JVS(1003)*XX(91)
  XX(34) = XX(34)-JVS(847)*XX(88)-JVS(1002)*XX(91)
  XX(33) = XX(33)-JVS(890)*XX(89)-JVS(1001)*XX(91)
  XX(32) = XX(32)-JVS(846)*XX(88)-JVS(889)*XX(89)-JVS(1000)*XX(91)-JVS(1136)*XX(94)-JVS(1184)*XX(95)
  XX(31) = XX(31)-JVS(320)*XX(57)-JVS(582)*XX(75)-JVS(888)*XX(89)-JVS(1183)*XX(95)
  XX(30) = XX(30)-JVS(767)*XX(85)-JVS(1135)*XX(94)-JVS(1182)*XX(95)-JVS(1235)*XX(96)
  XX(29) = XX(29)-JVS(999)*XX(91)-JVS(1134)*XX(94)-JVS(1181)*XX(95)
  XX(28) = XX(28)-JVS(461)*XX(69)-JVS(887)*XX(89)-JVS(998)*XX(91)
  XX(27) = XX(27)-JVS(886)*XX(89)-JVS(997)*XX(91)-JVS(1180)*XX(95)-JVS(1234)*XX(96)
  XX(26) = XX(26)-JVS(112)*XX(28)-JVS(311)*XX(56)-JVS(996)*XX(91)-JVS(1179)*XX(95)
  XX(25) = XX(25)-JVS(523)*XX(73)-JVS(885)*XX(89)-JVS(949)*XX(90)-JVS(995)*XX(91)
  XX(24) = XX(24)-JVS(156)*XX(34)-JVS(195)*XX(39)-JVS(675)*XX(80)-JVS(845)*XX(88)-JVS(884)*XX(89)-JVS(994)*XX(91)
  XX(23) = XX(23)-JVS(155)*XX(34)-JVS(189)*XX(38)-JVS(674)*XX(80)-JVS(844)*XX(88)-JVS(883)*XX(89)-JVS(993)*XX(91)
  XX(22) = XX(22)-JVS(270)*XX(52)-JVS(522)*XX(73)-JVS(992)*XX(91)-JVS(1178)*XX(95)
  XX(21) = XX(21)-JVS(410)*XX(66)-JVS(628)*XX(78)-JVS(882)*XX(89)-JVS(991)*XX(91)
  XX(20) = XX(20)-JVS(310)*XX(56)-JVS(990)*XX(91)
  XX(19) = XX(19)-JVS(843)*XX(88)-JVS(989)*XX(91)
  XX(18) = XX(18)-JVS(842)*XX(88)-JVS(1177)*XX(95)
  XX(17) = XX(17)-JVS(1176)*XX(95)-JVS(1233)*XX(96)
  XX(16) = XX(16)-JVS(385)*XX(64)
  XX(15) = XX(15)-JVS(881)*XX(89)-JVS(988)*XX(91)
  XX(14) = XX(14)-JVS(126)*XX(30)-JVS(987)*XX(91)
  XX(13) = XX(13)-JVS(841)*XX(88)
  XX(12) = XX(12)
  XX(11) = XX(11)
  XX(10) = XX(10)
  XX(9) = XX(9)
  XX(8) = XX(8)
  XX(7) = XX(7)
  XX(6) = XX(6)
  XX(5) = XX(5)
  XX(4) = XX(4)
  XX(3) = XX(3)
  XX(2) = XX(2)
  XX(1) = XX(1)
      
END SUBROUTINE KppSolveTR

! End of KppSolveTR function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



END MODULE racm_soa_vbs_het_LinearAlgebra


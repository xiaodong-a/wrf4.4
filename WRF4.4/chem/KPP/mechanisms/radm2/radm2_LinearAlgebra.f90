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
! File                 : radm2_LinearAlgebra.f90
! Time                 : Mon Sep 26 23:37:45 2022
! Working directory    : /home/ess/xiaodong/WORK/WRFCHEM/model/WRF4.4/chem/KPP/mechanisms/radm2
! Equation file        : radm2.kpp
! Output root filename : radm2
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE radm2_LinearAlgebra

  USE radm2_Parameters
  USE radm2_JacobianSP

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
  XX(3) = X(3)/JVS(10)
  XX(4) = X(4)/JVS(27)
  XX(5) = (X(5)-JVS(2)*XX(1))/(JVS(30))
  XX(6) = X(6)/JVS(32)
  XX(7) = X(7)/JVS(34)
  XX(8) = X(8)/JVS(36)
  XX(9) = X(9)/JVS(38)
  XX(10) = X(10)/JVS(40)
  XX(11) = X(11)/JVS(42)
  XX(12) = X(12)/JVS(44)
  XX(13) = X(13)/JVS(47)
  XX(14) = X(14)/JVS(50)
  XX(15) = X(15)/JVS(53)
  XX(16) = X(16)/JVS(56)
  XX(17) = X(17)/JVS(58)
  XX(18) = X(18)/JVS(63)
  XX(19) = X(19)/JVS(68)
  XX(20) = X(20)/JVS(72)
  XX(21) = X(21)/JVS(76)
  XX(22) = X(22)/JVS(82)
  XX(23) = X(23)/JVS(85)
  XX(24) = (X(24)-JVS(5)*XX(2))/(JVS(89))
  XX(25) = X(25)/JVS(95)
  XX(26) = (X(26)-JVS(28)*XX(4))/(JVS(106))
  XX(27) = (X(27)-JVS(6)*XX(2)-JVS(11)*XX(3)-JVS(107)*XX(26))/(JVS(121))
  XX(28) = (X(28)-JVS(7)*XX(2)-JVS(12)*XX(3)-JVS(59)*XX(17)-JVS(108)*XX(26))/(JVS(125))
  XX(29) = (X(29)-JVS(8)*XX(2)-JVS(13)*XX(3)-JVS(60)*XX(17)-JVS(109)*XX(26))/(JVS(129))
  XX(30) = (X(30)-JVS(96)*XX(25))/(JVS(133))
  XX(31) = (X(31)-JVS(97)*XX(25)-JVS(110)*XX(26))/(JVS(141))
  XX(32) = X(32)/JVS(150)
  XX(33) = X(33)/JVS(159)
  XX(34) = (X(34)-JVS(98)*XX(25)-JVS(111)*XX(26))/(JVS(171))
  XX(35) = (X(35)-JVS(134)*XX(30)-JVS(142)*XX(31)-JVS(172)*XX(34))/(JVS(182))
  XX(36) = (X(36)-JVS(135)*XX(30)-JVS(173)*XX(34))/(JVS(189))
  XX(37) = (X(37)-JVS(14)*XX(3))/(JVS(197))
  XX(38) = (X(38)-JVS(15)*XX(3))/(JVS(209))
  XX(39) = X(39)/JVS(221)
  XX(40) = (X(40)-JVS(16)*XX(3))/(JVS(231))
  XX(41) = (X(41)-JVS(17)*XX(3)-JVS(160)*XX(33))/(JVS(240))
  XX(42) = X(42)/JVS(253)
  XX(43) = (X(43)-JVS(99)*XX(25)-JVS(112)*XX(26))/(JVS(284))
  XX(44) = (X(44)-JVS(18)*XX(3)-JVS(161)*XX(33)-JVS(254)*XX(42)-JVS(285)*XX(43))/(JVS(300))
  XX(45) = (X(45)-JVS(19)*XX(3)-JVS(45)*XX(12)-JVS(113)*XX(26)-JVS(143)*XX(31)-JVS(174)*XX(34)-JVS(222)*XX(39)-JVS(255)&
             &*XX(42)-JVS(286)*XX(43))/(JVS(311))
  XX(46) = (X(46)-JVS(9)*XX(2)-JVS(20)*XX(3)-JVS(35)*XX(7)-JVS(61)*XX(17)-JVS(69)*XX(19)-JVS(90)*XX(24)-JVS(114)*XX(26)&
             &-JVS(122)*XX(27)-JVS(126)*XX(28)-JVS(130)*XX(29)-JVS(162)*XX(33)-JVS(198)*XX(37)-JVS(210)*XX(38)-JVS(232)&
             &*XX(40)-JVS(256)*XX(42)-JVS(287)*XX(43))/(JVS(324))
  XX(47) = (X(47)-JVS(163)*XX(33))/(JVS(335))
  XX(48) = (X(48)-JVS(100)*XX(25)-JVS(115)*XX(26))/(JVS(357))
  XX(49) = (X(49)-JVS(21)*XX(3)-JVS(164)*XX(33)-JVS(257)*XX(42)-JVS(288)*XX(43)-JVS(358)*XX(48))/(JVS(371))
  XX(50) = (X(50)-JVS(22)*XX(3)-JVS(175)*XX(34)-JVS(258)*XX(42)-JVS(289)*XX(43)-JVS(359)*XX(48))/(JVS(385))
  XX(51) = (X(51)-JVS(51)*XX(14)-JVS(64)*XX(18)-JVS(73)*XX(20)-JVS(77)*XX(21)-JVS(101)*XX(25)-JVS(151)*XX(32)-JVS(183)&
             &*XX(35)-JVS(190)*XX(36)-JVS(199)*XX(37)-JVS(211)*XX(38)-JVS(223)*XX(39)-JVS(233)*XX(40)-JVS(241)*XX(41)&
             &-JVS(259)*XX(42)-JVS(290)*XX(43)-JVS(301)*XX(44)-JVS(312)*XX(45)-JVS(325)*XX(46)-JVS(336)*XX(47)-JVS(360)&
             &*XX(48)-JVS(372)*XX(49)-JVS(386)*XX(50))/(JVS(427))
  XX(52) = (X(52)-JVS(23)*XX(3)-JVS(78)*XX(21)-JVS(116)*XX(26)-JVS(136)*XX(30)-JVS(144)*XX(31)-JVS(152)*XX(32)-JVS(165)&
             &*XX(33)-JVS(176)*XX(34)-JVS(184)*XX(35)-JVS(191)*XX(36)-JVS(200)*XX(37)-JVS(212)*XX(38)-JVS(224)*XX(39)&
             &-JVS(234)*XX(40)-JVS(242)*XX(41)-JVS(260)*XX(42)-JVS(291)*XX(43)-JVS(302)*XX(44)-JVS(313)*XX(45)-JVS(337)&
             &*XX(47)-JVS(361)*XX(48)-JVS(373)*XX(49)-JVS(387)*XX(50)-JVS(428)*XX(51))/(JVS(457))
  XX(53) = (X(53)-JVS(3)*XX(1)-JVS(29)*XX(4)-JVS(31)*XX(5)-JVS(33)*XX(6)-JVS(37)*XX(8)-JVS(39)*XX(9)-JVS(41)*XX(10)&
             &-JVS(43)*XX(11)-JVS(48)*XX(13)-JVS(52)*XX(14)-JVS(57)*XX(16)-JVS(62)*XX(17)-JVS(65)*XX(18)-JVS(74)*XX(20)&
             &-JVS(79)*XX(21)-JVS(83)*XX(22)-JVS(86)*XX(23)-JVS(91)*XX(24)-JVS(102)*XX(25)-JVS(117)*XX(26)-JVS(123)*XX(27)&
             &-JVS(127)*XX(28)-JVS(131)*XX(29)-JVS(137)*XX(30)-JVS(145)*XX(31)-JVS(153)*XX(32)-JVS(166)*XX(33)-JVS(177)&
             &*XX(34)-JVS(185)*XX(35)-JVS(192)*XX(36)-JVS(201)*XX(37)-JVS(213)*XX(38)-JVS(225)*XX(39)-JVS(235)*XX(40)&
             &-JVS(243)*XX(41)-JVS(261)*XX(42)-JVS(292)*XX(43)-JVS(303)*XX(44)-JVS(314)*XX(45)-JVS(326)*XX(46)-JVS(338)&
             &*XX(47)-JVS(362)*XX(48)-JVS(374)*XX(49)-JVS(388)*XX(50)-JVS(429)*XX(51)-JVS(458)*XX(52))/(JVS(505))
  XX(54) = (X(54)-JVS(54)*XX(15)-JVS(70)*XX(19)-JVS(84)*XX(22)-JVS(92)*XX(24)-JVS(103)*XX(25)-JVS(118)*XX(26)-JVS(124)&
             &*XX(27)-JVS(128)*XX(28)-JVS(132)*XX(29)-JVS(138)*XX(30)-JVS(146)*XX(31)-JVS(154)*XX(32)-JVS(167)*XX(33)&
             &-JVS(178)*XX(34)-JVS(202)*XX(37)-JVS(214)*XX(38)-JVS(226)*XX(39)-JVS(236)*XX(40)-JVS(262)*XX(42)-JVS(293)&
             &*XX(43)-JVS(315)*XX(45)-JVS(327)*XX(46)-JVS(339)*XX(47)-JVS(363)*XX(48)-JVS(375)*XX(49)-JVS(389)*XX(50)&
             &-JVS(430)*XX(51)-JVS(459)*XX(52)-JVS(506)*XX(53))/(JVS(536))
  XX(55) = (X(55)-JVS(24)*XX(3)-JVS(66)*XX(18)-JVS(87)*XX(23)-JVS(119)*XX(26)-JVS(139)*XX(30)-JVS(147)*XX(31)-JVS(155)&
             &*XX(32)-JVS(168)*XX(33)-JVS(179)*XX(34)-JVS(186)*XX(35)-JVS(193)*XX(36)-JVS(203)*XX(37)-JVS(215)*XX(38)&
             &-JVS(227)*XX(39)-JVS(237)*XX(40)-JVS(244)*XX(41)-JVS(263)*XX(42)-JVS(294)*XX(43)-JVS(304)*XX(44)-JVS(316)&
             &*XX(45)-JVS(340)*XX(47)-JVS(364)*XX(48)-JVS(376)*XX(49)-JVS(390)*XX(50)-JVS(431)*XX(51)-JVS(460)*XX(52)&
             &-JVS(507)*XX(53)-JVS(537)*XX(54))/(JVS(565))
  XX(56) = (X(56)-JVS(25)*XX(3)-JVS(169)*XX(33)-JVS(264)*XX(42)-JVS(295)*XX(43)-JVS(341)*XX(47)-JVS(365)*XX(48)-JVS(391)&
             &*XX(50)-JVS(432)*XX(51)-JVS(461)*XX(52)-JVS(508)*XX(53)-JVS(538)*XX(54)-JVS(566)*XX(55))/(JVS(583))
  XX(57) = (X(57)-JVS(26)*XX(3)-JVS(265)*XX(42)-JVS(296)*XX(43)-JVS(366)*XX(48)-JVS(433)*XX(51)-JVS(462)*XX(52)-JVS(509)&
             &*XX(53)-JVS(539)*XX(54)-JVS(567)*XX(55)-JVS(584)*XX(56))/(JVS(600))
  XX(58) = (X(58)-JVS(49)*XX(13)-JVS(120)*XX(26)-JVS(140)*XX(30)-JVS(148)*XX(31)-JVS(170)*XX(33)-JVS(180)*XX(34)&
             &-JVS(187)*XX(35)-JVS(194)*XX(36)-JVS(204)*XX(37)-JVS(216)*XX(38)-JVS(228)*XX(39)-JVS(238)*XX(40)-JVS(245)&
             &*XX(41)-JVS(266)*XX(42)-JVS(297)*XX(43)-JVS(305)*XX(44)-JVS(317)*XX(45)-JVS(328)*XX(46)-JVS(342)*XX(47)&
             &-JVS(367)*XX(48)-JVS(377)*XX(49)-JVS(392)*XX(50)-JVS(434)*XX(51)-JVS(463)*XX(52)-JVS(510)*XX(53)-JVS(540)&
             &*XX(54)-JVS(568)*XX(55)-JVS(585)*XX(56)-JVS(601)*XX(57))/(JVS(624))
  XX(59) = (X(59)-JVS(46)*XX(12)-JVS(55)*XX(15)-JVS(71)*XX(19)-JVS(75)*XX(20)-JVS(88)*XX(23)-JVS(104)*XX(25)-JVS(156)&
             &*XX(32)-JVS(229)*XX(39)-JVS(267)*XX(42)-JVS(298)*XX(43)-JVS(318)*XX(45)-JVS(329)*XX(46)-JVS(343)*XX(47)&
             &-JVS(368)*XX(48)-JVS(378)*XX(49)-JVS(393)*XX(50)-JVS(435)*XX(51)-JVS(464)*XX(52)-JVS(511)*XX(53)-JVS(541)&
             &*XX(54)-JVS(569)*XX(55)-JVS(586)*XX(56)-JVS(602)*XX(57)-JVS(625)*XX(58))/(JVS(659))
  XX(59) = XX(59)
  XX(58) = XX(58)-JVS(658)*XX(59)
  XX(57) = XX(57)-JVS(623)*XX(58)-JVS(657)*XX(59)
  XX(56) = XX(56)-JVS(599)*XX(57)-JVS(622)*XX(58)-JVS(656)*XX(59)
  XX(55) = XX(55)-JVS(582)*XX(56)-JVS(598)*XX(57)-JVS(621)*XX(58)-JVS(655)*XX(59)
  XX(54) = XX(54)-JVS(564)*XX(55)-JVS(581)*XX(56)-JVS(597)*XX(57)-JVS(620)*XX(58)-JVS(654)*XX(59)
  XX(53) = XX(53)-JVS(535)*XX(54)-JVS(563)*XX(55)-JVS(580)*XX(56)-JVS(596)*XX(57)-JVS(619)*XX(58)-JVS(653)*XX(59)
  XX(52) = XX(52)-JVS(504)*XX(53)-JVS(534)*XX(54)-JVS(562)*XX(55)-JVS(579)*XX(56)-JVS(595)*XX(57)-JVS(618)*XX(58)&
             &-JVS(652)*XX(59)
  XX(51) = XX(51)-JVS(456)*XX(52)-JVS(503)*XX(53)-JVS(533)*XX(54)-JVS(561)*XX(55)-JVS(578)*XX(56)-JVS(594)*XX(57)&
             &-JVS(617)*XX(58)-JVS(651)*XX(59)
  XX(50) = XX(50)-JVS(426)*XX(51)-JVS(455)*XX(52)-JVS(502)*XX(53)-JVS(532)*XX(54)-JVS(560)*XX(55)-JVS(577)*XX(56)&
             &-JVS(616)*XX(58)-JVS(650)*XX(59)
  XX(49) = XX(49)-JVS(384)*XX(50)-JVS(425)*XX(51)-JVS(454)*XX(52)-JVS(501)*XX(53)-JVS(531)*XX(54)-JVS(559)*XX(55)&
             &-JVS(576)*XX(56)-JVS(593)*XX(57)-JVS(615)*XX(58)-JVS(649)*XX(59)
  XX(48) = XX(48)-JVS(424)*XX(51)-JVS(453)*XX(52)-JVS(500)*XX(53)-JVS(530)*XX(54)-JVS(558)*XX(55)-JVS(648)*XX(59)
  XX(47) = XX(47)-JVS(356)*XX(48)-JVS(383)*XX(50)-JVS(423)*XX(51)-JVS(499)*XX(53)-JVS(557)*XX(55)-JVS(575)*XX(56)&
             &-JVS(592)*XX(57)-JVS(647)*XX(59)
  XX(46) = XX(46)-JVS(334)*XX(47)-JVS(355)*XX(48)-JVS(370)*XX(49)-JVS(382)*XX(50)-JVS(422)*XX(51)-JVS(452)*XX(52)&
             &-JVS(498)*XX(53)-JVS(529)*XX(54)-JVS(556)*XX(55)-JVS(574)*XX(56)-JVS(591)*XX(57)-JVS(614)*XX(58)-JVS(646)&
             &*XX(59)
  XX(45) = XX(45)-JVS(354)*XX(48)-JVS(421)*XX(51)-JVS(451)*XX(52)-JVS(497)*XX(53)-JVS(528)*XX(54)-JVS(555)*XX(55)&
             &-JVS(573)*XX(56)-JVS(613)*XX(58)-JVS(645)*XX(59)
  XX(44) = XX(44)-JVS(333)*XX(47)-JVS(353)*XX(48)-JVS(381)*XX(50)-JVS(420)*XX(51)-JVS(450)*XX(52)-JVS(496)*XX(53)&
             &-JVS(527)*XX(54)-JVS(554)*XX(55)-JVS(572)*XX(56)-JVS(590)*XX(57)-JVS(612)*XX(58)-JVS(644)*XX(59)
  XX(43) = XX(43)-JVS(419)*XX(51)-JVS(495)*XX(53)-JVS(526)*XX(54)-JVS(643)*XX(59)
  XX(42) = XX(42)-JVS(352)*XX(48)-JVS(418)*XX(51)-JVS(494)*XX(53)-JVS(571)*XX(56)
  XX(41) = XX(41)-JVS(252)*XX(42)-JVS(283)*XX(43)-JVS(332)*XX(47)-JVS(351)*XX(48)-JVS(380)*XX(50)-JVS(417)*XX(51)&
             &-JVS(449)*XX(52)-JVS(493)*XX(53)-JVS(553)*XX(55)-JVS(589)*XX(57)-JVS(611)*XX(58)-JVS(642)*XX(59)
  XX(40) = XX(40)-JVS(251)*XX(42)-JVS(282)*XX(43)-JVS(350)*XX(48)-JVS(416)*XX(51)-JVS(448)*XX(52)-JVS(552)*XX(55)&
             &-JVS(610)*XX(58)-JVS(641)*XX(59)
  XX(39) = XX(39)-JVS(250)*XX(42)-JVS(281)*XX(43)-JVS(415)*XX(51)-JVS(447)*XX(52)-JVS(551)*XX(55)-JVS(609)*XX(58)&
             &-JVS(640)*XX(59)
  XX(38) = XX(38)-JVS(280)*XX(43)-JVS(331)*XX(47)-JVS(349)*XX(48)-JVS(414)*XX(51)-JVS(446)*XX(52)-JVS(550)*XX(55)&
             &-JVS(608)*XX(58)-JVS(639)*XX(59)
  XX(37) = XX(37)-JVS(249)*XX(42)-JVS(279)*XX(43)-JVS(348)*XX(48)-JVS(413)*XX(51)-JVS(445)*XX(52)-JVS(549)*XX(55)&
             &-JVS(607)*XX(58)-JVS(638)*XX(59)
  XX(36) = XX(36)-JVS(248)*XX(42)-JVS(278)*XX(43)-JVS(310)*XX(45)-JVS(412)*XX(51)-JVS(444)*XX(52)-JVS(492)*XX(53)&
             &-JVS(525)*XX(54)-JVS(548)*XX(55)-JVS(606)*XX(58)-JVS(637)*XX(59)
  XX(35) = XX(35)-JVS(247)*XX(42)-JVS(277)*XX(43)-JVS(309)*XX(45)-JVS(411)*XX(51)-JVS(443)*XX(52)-JVS(491)*XX(53)&
             &-JVS(524)*XX(54)-JVS(547)*XX(55)-JVS(605)*XX(58)-JVS(636)*XX(59)
  XX(34) = XX(34)-JVS(410)*XX(51)-JVS(490)*XX(53)-JVS(523)*XX(54)-JVS(546)*XX(55)-JVS(635)*XX(59)
  XX(33) = XX(33)-JVS(379)*XX(50)-JVS(489)*XX(53)-JVS(545)*XX(55)-JVS(588)*XX(57)
  XX(32) = XX(32)-JVS(246)*XX(42)-JVS(276)*XX(43)-JVS(330)*XX(47)-JVS(409)*XX(51)-JVS(442)*XX(52)-JVS(544)*XX(55)&
             &-JVS(634)*XX(59)
  XX(31) = XX(31)-JVS(275)*XX(43)-JVS(408)*XX(51)-JVS(488)*XX(53)-JVS(522)*XX(54)-JVS(633)*XX(59)
  XX(30) = XX(30)-JVS(308)*XX(45)-JVS(407)*XX(51)-JVS(487)*XX(53)-JVS(521)*XX(54)-JVS(632)*XX(59)
  XX(29) = XX(29)-JVS(158)*XX(33)-JVS(208)*XX(38)-JVS(274)*XX(43)-JVS(323)*XX(46)-JVS(347)*XX(48)-JVS(369)*XX(49)&
             &-JVS(406)*XX(51)-JVS(441)*XX(52)-JVS(486)*XX(53)-JVS(520)*XX(54)
  XX(28) = XX(28)-JVS(196)*XX(37)-JVS(207)*XX(38)-JVS(273)*XX(43)-JVS(322)*XX(46)-JVS(346)*XX(48)-JVS(405)*XX(51)&
             &-JVS(440)*XX(52)-JVS(485)*XX(53)-JVS(519)*XX(54)
  XX(27) = XX(27)-JVS(195)*XX(37)-JVS(206)*XX(38)-JVS(272)*XX(43)-JVS(321)*XX(46)-JVS(345)*XX(48)-JVS(404)*XX(51)&
             &-JVS(439)*XX(52)-JVS(484)*XX(53)-JVS(518)*XX(54)
  XX(26) = XX(26)-JVS(403)*XX(51)-JVS(483)*XX(53)
  XX(25) = XX(25)-JVS(482)*XX(53)-JVS(517)*XX(54)-JVS(631)*XX(59)
  XX(24) = XX(24)-JVS(105)*XX(26)-JVS(205)*XX(38)-JVS(230)*XX(40)-JVS(271)*XX(43)-JVS(320)*XX(46)-JVS(402)*XX(51)&
             &-JVS(481)*XX(53)-JVS(516)*XX(54)
  XX(23) = XX(23)-JVS(220)*XX(39)-JVS(270)*XX(43)-JVS(480)*XX(53)-JVS(515)*XX(54)-JVS(543)*XX(55)-JVS(630)*XX(59)
  XX(22) = XX(22)-JVS(94)*XX(25)-JVS(149)*XX(32)-JVS(219)*XX(39)-JVS(307)*XX(45)-JVS(401)*XX(51)-JVS(479)*XX(53)&
             &-JVS(514)*XX(54)
  XX(21) = XX(21)-JVS(269)*XX(43)-JVS(400)*XX(51)-JVS(438)*XX(52)-JVS(478)*XX(53)
  XX(20) = XX(20)-JVS(399)*XX(51)-JVS(477)*XX(53)-JVS(513)*XX(54)-JVS(629)*XX(59)
  XX(19) = XX(19)-JVS(319)*XX(46)-JVS(604)*XX(58)-JVS(628)*XX(59)
  XX(18) = XX(18)-JVS(437)*XX(52)-JVS(476)*XX(53)-JVS(542)*XX(55)
  XX(17) = XX(17)-JVS(436)*XX(52)-JVS(475)*XX(53)
  XX(16) = XX(16)-JVS(157)*XX(33)-JVS(268)*XX(43)-JVS(344)*XX(48)-JVS(398)*XX(51)-JVS(474)*XX(53)-JVS(570)*XX(56)
  XX(15) = XX(15)-JVS(93)*XX(25)-JVS(512)*XX(54)-JVS(627)*XX(59)
  XX(14) = XX(14)-JVS(397)*XX(51)-JVS(473)*XX(53)
  XX(13) = XX(13)-JVS(472)*XX(53)-JVS(603)*XX(58)
  XX(12) = XX(12)-JVS(306)*XX(45)-JVS(626)*XX(59)
  XX(11) = XX(11)-JVS(81)*XX(22)-JVS(188)*XX(36)-JVS(396)*XX(51)-JVS(471)*XX(53)
  XX(10) = XX(10)-JVS(80)*XX(22)-JVS(181)*XX(35)-JVS(395)*XX(51)-JVS(470)*XX(53)
  XX(9) = XX(9)-JVS(218)*XX(39)-JVS(299)*XX(44)-JVS(469)*XX(53)
  XX(8) = XX(8)-JVS(217)*XX(39)-JVS(239)*XX(41)-JVS(468)*XX(53)
  XX(7) = XX(7)-JVS(67)*XX(19)-JVS(467)*XX(53)
  XX(6) = XX(6)-JVS(466)*XX(53)-JVS(587)*XX(57)
  XX(5) = XX(5)-JVS(394)*XX(51)-JVS(465)*XX(53)
  XX(4) = XX(4)
  XX(3) = XX(3)
  XX(2) = XX(2)
  XX(1) = XX(1)
      
END SUBROUTINE KppSolveTR

! End of KppSolveTR function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



END MODULE radm2_LinearAlgebra


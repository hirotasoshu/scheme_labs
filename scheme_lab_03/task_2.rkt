#lang scheme
(define (pow2? n)
   (if (> n 0) (= (bitwise-and n (- n 1)) 0) #f))
(pow2? 6591020467154715500519448971182956815920095805466363845464017251208784640564172275539876061586061016570470378240358411519223408847120290640865531226256092019272337222193444478164485321385996965018293276462124757244416091694707916187810004642461304473055519289377038956632720459291562793025215823642301818245411925003913405836234450105344552244044742673596076354410059882503062972429198880937334650639372616752398293497257027217608526901866351620141700407963084674182399736358047288447003621584331284443301532916243415303062508821678234866203626437283608866847412118337661670877840919610977346910228036010126942185613413422429318097106437569783293722695034007373987906985874057385219624413344333564618977064277920937230332864815246329300089669136289555010440614383680600301739486560084446998787176863826999484611546399127850833033986349453118460814382386418957763043232194580785267308478351872420251487639547043793649484211758265813045449194675724510028425610539524483359439471654616035989287819938604310618520470063456383805329070539689379021701805712727372090288575427602468581730451256366275630269819610728502988623633338172009077823754757698355675743773031239722296389527777710681361252287853455474688
);проверка на 2^3999
(pow2? 6591020467154715500519448971182956815920095805466363845464017251208784640564172275539876061586061016570470378240358411519223408847120290640865531226256092019272337222193444478164485321385996965018293276462124757244416091694707916187810004642461304473055519289377038956632720459291562793025215823642301818245411925003913405836234450105344552244044742673596076354410059882503062972429198880937334650639372616752398293497257027217608526901866351620141700407963084674182399736358047288447003621584331284443301532916243415303062508821678234866203626437283608866847412118337661670877840919610977346910228036010126942185613413422429318097106437569783293722695034007373987906985874057385219624413344333564618977064277920937230332864815246329300089669136289555010440614383680600301739486560084446998787176863826999484611546399127850833033986349453118460814382386418957763043232194580785267308478351872420251487639547043793649484211758265813045449194675724510028425610539524483359439471654616035989287819938604310618520470063456383805329070539689379021701805712727372090288575427602468581730451256366275630269819610728502988623633338172009077823754757698355675743773031239722296389527777710681361252287853455474687
);проверка на 2^3999 - 1
(pow2? 1090748135619415929462984244733782862448264161996232692431832786189721331849119295216264234525201987223957291796157025273109870820177184063610979765077554799078906298842192989538609825228048205159696851613591638196771886542609324560121290553901886301017900252535799917200010079600026535836800905297805880952350501630195475653911005312364560014847426035293551245843928918752768696279344088055617515694349945406677825140814900616105920256438504578013326493565836047242407382442812245131517757519164899226365743722432277368075027627883045206501792761700945699168497257879683851737049996900961120515655050115561271491492515342105748966629547032786321505730828430221664970324396138635251626409516168005427623435996308921691446181187406395310665404885739434832877428167407495370993511868756359970390117021823616749458620969857006263612082706715408157066575137281027022310927564910276759160520878304632411049364568754920967322982459184763427383790272448438018526977764941072715611580434690827459339991961414242741410599117426060556483763756314527611362658628383368621157993638020878537675545336789915694234433955666315070087213535470255670312004130725495834508357439653828936077080978550578912967907352780054935621561090795845172954115972927479877527738560008204118558930004777748727761853813510493840581861598652211605960308356405941821189714037868726219481498727603653616298856174822413033485438785324024751419417183012281078209729303537372804574372095228703622776363945290869806258422355148507571039619387449629866808188769662815778153079393179093143648340761738581819563002994422790754955061288818308430079648693232179158765918035565216157115402992120276155607873107937477466841528362987708699450152031231862594203085693838944657061346236704234026821102958954951197087076546186622796294536451620756509351018906023773821539532776208676978589731966330308893304665169436185078350641568336944530051437491311298834367265238595404904273455928723949525227184617404367854754610474377019768025576605881038077270707717942221977090385438585844095492116099852538903974655703943973086090930596963360767529964938414598185705963754561497355827813623833288906309004288017321424808663962671333528009232758350873059614118723781422101460198615747386855096896089189180441339558524822867541113212638793675567650340362970031930023397828465318547238244232028015189689660418822976000815437610652254270163595650875433851147123214227266605403581781469090806576468950587661997186505665475715792896)
;проверка на 2^8192
(pow2? 1090748135619415929462984244733782862448264161996232692431832786189721331849119295216264234525201987223957291796157025273109870820177184063610979765077554799078906298842192989538609825228048205159696851613591638196771886542609324560121290553901886301017900252535799917200010079600026535836800905297805880952350501630195475653911005312364560014847426035293551245843928918752768696279344088055617515694349945406677825140814900616105920256438504578013326493565836047242407382442812245131517757519164899226365743722432277368075027627883045206501792761700945699168497257879683851737049996900961120515655050115561271491492515342105748966629547032786321505730828430221664970324396138635251626409516168005427623435996308921691446181187406395310665404885739434832877428167407495370993511868756359970390117021823616749458620969857006263612082706715408157066575137281027022310927564910276759160520878304632411049364568754920967322982459184763427383790272448438018526977764941072715611580434690827459339991961414242741410599117426060556483763756314527611362658628383368621157993638020878537675545336789915694234433955666315070087213535470255670312004130725495834508357439653828936077080978550578912967907352780054935621561090795845172954115972927479877527738560008204118558930004777748727761853813510493840581861598652211605960308356405941821189714037868726219481498727603653616298856174822413033485438785324024751419417183012281078209729303537372804574372095228703622776363945290869806258422355148507571039619387449629866808188769662815778153079393179093143648340761738581819563002994422790754955061288818308430079648693232179158765918035565216157115402992120276155607873107937477466841528362987708699450152031231862594203085693838944657061346236704234026821102958954951197087076546186622796294536451620756509351018906023773821539532776208676978589731966330308893304665169436185078350641568336944530051437491311298834367265238595404904273455928723949525227184617404367854754610474377019768025576605881038077270707717942221977090385438585844095492116099852538903974655703943973086090930596963360767529964938414598185705963754561497355827813623833288906309004288017321424808663962671333528009232758350873059614118723781422101460198615747386855096896089189180441339558524822867541113212638793675567650340362970031930023397828465318547238244232028015189689660418822976000815437610652254270163595650875433851147123214227266605403581781469090806576468950587661997186505665475715792895
);проверка на 2^8192 - 1
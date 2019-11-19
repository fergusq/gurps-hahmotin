(ns gurps-hahmotin.templates
  (:require [clojure.spec.alpha :as s]))

(s/def ::name string?)
(s/def ::skill (s/or :general keyword?
                     :specialized (s/tuple keyword? string?)))
(s/def ::used-CP (s/map-of ::skill number?))
(s/def ::optional-CP (s/coll-of (s/coll-of ::used-CP)))
(s/def ::lens (s/coll-of ::template))
(s/def ::notes (s/coll-of string?))

(s/def ::template (s/keys :req-un [::name]
                          :opt-un [::used-CP ::optional-CP ::lens ::notes]))

(s/def ::template-map (s/map-of keyword? ::template))

(defn- allocate-points-set [points skills]
  (set (if (= points 1)
         (for [skill skills]
           {skill 1})
         (for [skill skills
               allocation (allocate-points-set (dec points) skills)]
           (merge-with + {skill 1} allocation)))))

(defn- allocate-points [points skills]
  (->> (allocate-points-set points skills)
       (into [])
       (sort #(compare (str %1) (str %2)))))

(defn- allocate-same-to-each-set [points num skills]
  (set (if (= num 1)
         (for [skill skills]
           {skill points})
         (for [skill skills
               allocation (allocate-same-to-each-set points (dec num) (disj skills skill))]
           (merge {skill points} allocation)))))

(defn- allocate-same-to-each [points num skills]
  (->> (allocate-same-to-each-set points num skills)
       (into [])
       (sort #(compare (str %1) (str %2)))))

(def human {:name "Human"
            :species "Human"})

(def dwarf {:name "Dwarf"
            :species "Dwarf"
            :used-CP {:HT 10
                      :Will 5
                      :BM -5
                      :NightVision 4
                      :ExtendedLifespan 2
                      [:Resistant "Poison"] 3
                      :AlcoholTolerance 1}})

(def elf {:name "Elf"
          :species "Elf"
          :used-CP {:ST -10
                    :DX 20
                    :Per 5
                    :Magery 5
                    :ExtendedLifespan 6
                    :AcuteHearing 4
                    :GoodBalance 6
                    [:Appearance "Attractive"] 4}})

(def half-elf {:name "Half-Elf"
               :species "Half-Elf"
               :used-CP {:ST -10
                         :DX 20
                         :Per 5
                         :ExtendedLifespan 2
                         [:Appearance "Attractive"] 4
                         :DistinctiveFeatures -1}})

(def halfling {:name "Halfling"
               :species "Halfling"
               :used-CP {:ST -30
                         :DX 20
                         :HT 10
                         :Will 10
                         :MaxHP 2
                         :BM -5
                         :Silence 15
                         :HardtoKill 8}})

(def half-orc {:name "Half-Orc"
               :species "Half-Orc"
               :used-CP {:ST 20
                         :IQ -20
                         :HT 10
                         :MaxHP 2
                         :BM 5
                         :NightVision 3
                         :SocialStigmaSecond-ClassCitizen -5}
               :optional-CP [[{:BadTemper -5}
                              {:Impulsiveness -5}
                              {:Bloodlust -5}
                              {:DistinctiveFeatures -1
                               [:Appearance "Unattractive"] -4}]]})

(def ogreblood {:name "Ogreblood"
                :species "Ogreblood"
                :used-CP {:ST 40
                          :IQ -20
                          :HT 10
                          :NightVision 3
                          :SocialStigmaSavage -10
                          :DamageResistance 3
                          :Fearlessness 2
                          [:Appearance "Ugly"] -8}})

(def goblin {:name "Goblin"
             :species "Goblin"
             :used-CP {:IQ -20
                       :HT 10
                       :Per 5
                       :Will 5
                       :Infravision 10
                       :RapidHealing 5
                       [:Resistant "Metabolic Hazards"] 10
                       [:Teeth "Sharp"] 1
                       :SocialStigmaSavage -10
                       [:Appearance "Ugly"] -8
                       [:Language "Goblin"] 6
                       [:CulturalFamiliarity "Goblin"] 1}
             :optional-CP [[{:Cowardice -10}
                            {:Impulsiveness -10}
                            {:Bloodlust -10}
                            {:Kleptomania -10}]]})

(def species {:Human human
              :Dwarf dwarf
              :Elf elf
              :Half-Elf half-elf
              :Halfling halfling
              :Half-Orc half-orc
              :Ogreblood ogreblood
              :Goblin goblin})

(def nobility {:name "Nobility"
               :background "Nobility"
               :used-CP {:IQ 20
                         :ST -10
                         [:Wealth "Comfortable"] 10
                         [:Status "Minor nobility"] 10
                         [:Duty "Liege lord"] -5
                         :Rapier 1
                         [:Riding "Equines"] 1
                         :Diplomacy 1
                         [:Savoir-Faire "High Society"] 1
                         :Leadership 1
                         [:AreaKnowledge "Kingdom"] 1
                         [:CurrentAffairs "People"] 1
                         [:History "Kingdom"] 1
                         :Heraldry 1}
               :optional-CP [[{:Gluttony -5}
                              {:Chummy -5}
                              {[:Intolerance "Commoners"] -5}
                              {:Overconfidence -5}
                              {:Debt -5}
                              {:Enemy -5}]
                             [{:Carousing 1}
                              {:Gambling 1}
                              {:MusicalInstrument 1}
                              {:Hobby 1}]]})

(def tradesperson {:name "Tradesperson"
                   :background "Tradesperson"
                   :used-CP {:Stealth 1
                             :Streetwise 1
                             :Fast-Talk 1
                             :Climbing 1
                             :FirstAid 1
                             [:Teamster "Equines"] 1
                             :Merchant 2
                             [:CurrentAffairs "Home town"] 1}
                   :optional-CP [[{:DX 20}
                                  {:IQ 20}]
                                 [{[:Contact "Relevant Crafts Guild"] 5}
                                  {:SignatureGear 5}
                                  {:Per 5}
                                  {:HighManualDexterity 5}]
                                 [{[:Wealth "Struggling"] -10}
                                  {:Will -10}
                                  {:Dependent -10}
                                  {:Miserliness -10}
                                  {:Honesty -10}]
                                 [{:Brawling 1}
                                  {:Wrestling 1}
                                  {:Knife 1}]
                                 [{:Broadsword 1}
                                  {:Staff 1}
                                  {:AxeMace 1}]
                                 [{:Merchant 4}
                                  {[:Smith "Iron"] 4}
                                  {:Carpentry 4}
                                  {:Masonry 4}
                                  {:Leatherworking 4}
                                  {:Jeweler 4}
                                  {:Sewing 4}
                                  {:Cooking 4}
                                  {:Engineer 4}]]})

(def street-rat {:name "Street Rat"
                 :background "Street Rat"
                 :used-CP {:Stealth 1
                           :Acrobatics 1
                           :Climbing 1
                           :Swimming 1
                           :Throwing 1
                           :Fast-Talk 1
                           :Intimidation 1
                           :Streetwise 2
                           [:AreaKnowledge "Home town"] 2
                           :FirstAid 1
                           :Scrounging 1}
                 :optional-CP [[{[:Wealth "Struggling"] -10}
                                {:Status -10}
                                {:HT -10}
                                {:ShortAttentionSpan -10}
                                {:Impulsiveness -10}
                                {:OdiousPersonalHabit -10}
                                {:DistinctiveFeatures -2
                                 [:Appearance "Ugly"] -8}
                                {:SocialStigmaUneducated -5
                                 :Delusion -5}]
                               (allocate-points 2 [:Brawling
                                                   :Wrestling
                                                   :Knife
                                                   [:ThrownWeapon "Knife"]])
                               [{:Broadsword 1}
                                {:Staff 1}
                                {:AxeMace 1}]
                               (allocate-points 2 [:Pickpocket
                                                   :SleightofHand
                                                   :Forgery
                                                   :ForcedEntry
                                                   :Merchant])
                               (allocate-points 2 [:Acting
                                                   :Disguise
                                                   :MusicalInstrument
                                                   :Panhandling
                                                   :Performance])]})

(def rural {:name "Rural"
            :background "Rural"
            :used-CP {:Stealth 1
                      :Climbing 1
                      :Swimming 1
                      :FirstAid 1
                      :Survival 1
                      :WeatherSense 1
                      :Farming 1
                      :AnimalHandling 2}
            :optional-CP [[{[:Contact "Extended Family"] 5}
                           {:AnimalEmpathy 5}
                           {:Fit 5}
                           {:SignatureGear 5}
                           {:Per 5}]
                          [{[:SenseofDuty "Extended Family"] -5}
                           {:Truthfulness -5}
                           {:Gullibility -5}
                           {:Delusion -5}
                           {:Status -5}
                           {:Will -5}
                           {:Post-CombatShakes -5}]
                          [{:Hiking 1}
                           {:Riding 1}
                           {:Teamster 1}]
                          [{:Brawling 1}
                           {:Wrestling 1}
                           {:Throwing 1}
                           {:Fast-Talk 1}]
                          [{:Staff 1}
                           {:AxeMace 1}
                           {:Spear 1}
                           {:Running 1}]
                          [{:Bow 2}
                           {:Sling 2}
                           {:Traps 2}]
                          (allocate-same-to-each 2 3 #{:Scrounging
                                                       [:Smith "Iron"]
                                                       :Carpentry
                                                       :Leatherworking
                                                       :Sewing
                                                       :Cooking
                                                       :Veterinary})]})

(def lens-clan {:name "Barbarian/Orc Clan"
                :used-CP {:ST 10
                          [:Language "Barbarian / Orc"] 3
                          [:CulturalFamiliarity "Barbarian / Orc"] 1
                          :Intimidation 1
                          :Brawling 1
                          :Broadsword 1
                          :Shield 1
                          :Riding 1
                          :AnimalHandling 1}
                :optional {:background ["Barbarian Clan"
                                        "Orc Clan"]}})

(def lens-elf-groove {:name "Elf Groove"
                      :background "Elf Groove"
                      :used-CP {:Per 5
                                [:Language "Elvish"] 6
                                [:CulturalFamiliarity "Elvish"] 1
                                :Acrobatics 2
                                :Bow 1
                                :Diplomacy 2
                                :Naturalist 1
                                [:HiddenLore "Faerie"] 1
                                :MusicalInstrument 1}})

(def wilderness {:name "Wilderness"
                 :background "Wilderness"
                 :used-CP {:DX 20
                           :Stealth 2
                           :Climbing 1
                           :Swimming 1
                           :FirstAid 1
                           :Survival 1
                           :Naturalist 1
                           :Tracking 1
                           [:Navigation "Land"] 1
                           :Hiking 1
                           [:AreaKnowledge "sizeable wilderness"] 1
                           :Knife 1
                           :Bow 1}
                 :optional-CP [[{[:SenseofDuty "Adventuring Party"] -5}
                                {:Loner -5}
                                {[:Secret "Serious Embarrasment"] -5}
                                {:OdiousPersonalHabit -5}
                                {:Status -5}
                                {[:LightSleeper "Urban Areas"] -5}
                                {:MinorAddiction -5}]
                               [{:HerbLore 1}
                                {:Poisons 1}
                                {:Traps 1}
                                {[:Mimicry "animal & bird noises"] 1}
                                {:Camouflage 1}]
                               [{:Scrounging 1}
                                {:Leatherworking 1}
                                {:Cooking 1}
                                {:Fishing 1}]]
                 :lens [lens-clan lens-elf-groove]})
                                 
(def dwarf-clan {:name "Dwarf Clan"
                 :background "Dwarf Clan"
                 :used-CP {:Will 5
                           [:CulturalFamiliarity "Dwarven"] 1
                           [:Language "Dwarven"] 1
                           :NightVision 1
                           :Stealth 1
                           :Climbing 1
                           :FirstAid 1
                           [:Survival "Mountains"] 1
                           :Prospecting 2
                           :Hiking 1
                           :Carousing 1
                           :AreaKnowledge 1
                           [:CurrentAffairs "Dwarves"] 2}
                 :optional-CP [[{:CodeofHonorProfessional -5}
                                {:Truthfulness -5}
                                {:Honesty -5}
                                {:Chummy -5}
                                {:Obsession -5}
                                {:Status -5}]
                               [{:Brawling 1}
                                {:Wrestling 1}
                                {:Knife 1}]
                               [{:AxeMace 2}
                                {:Two-HandedAxeMace 2}]
                               [{[:ThrownWeapon "Axe"] 2}
                                {:Crossbow 2}]
                               (allocate-same-to-each 2 3 #{:ForcedEntry
                                                            :Scrounging
                                                            :Merchant
                                                            :Archaeology
                                                            :Traps
                                                            :Masonry
                                                            [:Smith "Iron"]
                                                            :Jeweler
                                                            :Engineer})]})

(def backgrounds {:Nobility nobility
                  :Tradesperson tradesperson
                  :StreetRat street-rat
                  :Rural rural
                  :Wilderness wilderness
                  :DwarfClan dwarf-clan})

(def lens-cavalry {:name "Cavalry"
                   :profession "Cavalry"
                   :used-CP {[:Riding "Equines"] 4
                             :Spear 1
                             :AnimalHandling 2
                             :Veterinary 1
                             :Tactics 2}})

(def lens-officer {:name "Officer"
                   :profession "Officer"
                   :used-CP {:Tactics 2
                             :Leadership 2
                             :Administration 2
                             [:Savoir-Faire "High Society"] 2
                             :Intimidate 1}})

(def lens-veteran {:name "Veteran"
                   :profession "Veteran"
                   :used-CP {[:Savoir-Faire "Military"] 2
                             :Scrounging 2
                             [:Contact "Old Teammates"] 2}
                   :notes ["Distribute 4 CP among weapn skills you know"]})

(def lens-combat-engineer {:name "Combat Engineer"
                           :profession "Combat Engineer"
                           :used-CP {[:Engineer "Combat"] 2
                                     :ForcedEntry 2
                                     :Explosives 2
                                     [:Smith "Iron"] 1
                                     :Carpentry 1
                                     :Traps 2}})

(def soldier {:name "Soldier"
              :profession "Soldier"
              :used-CP {:ST 10
                        :Broadsword 2
                        :Shield 2
                        :Spear 1
                        :Crossbow 1
                        :Hiking 2
                        :FirstAid 1
                        [:Savoir-Faire "Military"] 2
                        :Heraldry 1}
              :optional-CP [[{:CombatReflexes 15}
                             {:Unfazeable 15}
                             {:RapidHealingVery 15}
                             {:ST 10
                              :MaxHP 2
                              :MaxFP 3}]
                            [{:Duty -10
                              :Rank 5}
                             {:Bloodlust -5}
                             {:Post-CombatShakes -5}
                             {:Wounded -5}
                             {:Secret -5}
                             {:Will -5}]
                            (allocate-same-to-each 2 2 #{:Teamster
                                                         :Tactics
                                                         :ForcedEntry
                                                         :FirstAid
                                                         :Surgery})
                            (allocate-points 4 [:Brawling
                                                :Cooking
                                                :Fast-Talk
                                                :Carousing
                                                :Gambling
                                                :Scrounging
                                                :AreaKnowledge])]
              :lens [lens-cavalry lens-officer lens-veteran lens-combat-engineer]})

(def lens-conman {:name "Conman"
                  :profession "Conman"
                  :used-CP {:Acting 2
                            :Fast-Talk 4
                            :Forgery 1
                            [:Savoir-Faire "High Society"] 1
                            :Disguise 2}})

(def lens-assassin {:name "Assassin"
                    :profession "Assassin"
                    :used-CP {:Poisons 2
                              :Stealth 3
                              :HerbLore 1}
                    :notes ["Add 4 CP to one weapon skill"]})

(def lens-burglar {:name "Burglar"
                   :profession "Burglar"
                   :used-CP {:Climbing 2
                             :Lockpicking 2
                             :ForcedEntry 4
                             :Acrobatics 1
                             :Jumping 1}})

(def lens-fingers {:name "Fingers"
                   :profession "Fingers"
                   :used-CP {:Pickpocket 2
                             :SleightofHand 4
                             :Holdout 1
                             :Acting 2
                             :Fast-Talk 1}})

(def thief {:name "Thief"
            :profession "Thief"
            :used-CP {:Stealth 1
                      :Acrobatics 1
                      :Climbing 1
                      :FirstAid 1
                      :Throwing 1
                      :AreaKnowledge 1
                      :Holdout 1
                      :Streetwise 1}
            :optional-CP [[{:DX 20}
                           {:IQ 20}]
                          [{:DX 20
                            :HT -10}
                           {:Catfall 10}
                           {:NightVision 3
                            :AcuteHearing 2
                            :Per 5}
                           {[:Contact "A group"] 10}
                           {:CombatReflexes 15
                            :LightSleeper -5}]
                          [{[:Enemy "Past Victims"] -10}
                           {:Laziness -10}
                           {:Cowardice -10}
                           {:Kleptomania -10}
                           {[:Debt "10 silver coins / month"] -10}
                           {:Status -10}]
                          (allocate-same-to-each 2 2 #{:Lockpicking
                                                       :Pickpocket
                                                       :Intimidation
                                                       :Scrounging
                                                       :Acrobatics
                                                       :Disguise})
                          (allocate-same-to-each 1 2 #{:Merchant
                                                       [:Law "Kingdom"]
                                                       :Carousing
                                                       :Gambling
                                                       :Camouflage
                                                       :Fast-Draw})]
            :notes ["Add 2 CP to one melee weapon skill"
                    "Add 2 CP to one ranged weapon skill"]
            :lens [lens-conman lens-assassin lens-burglar lens-fingers]})

(def lens-hunter {:name "Hunter"
                  :profession "Hunter"
                  :used-CP {:Traps 2
                            :Bow 2
                            :Tracking 2
                            :Leatherworking 2
                            :Naturalist 1
                            :Cooking 1}})

(def lens-scout {:name "Scout"
                 :profession "Scout"
                 :used-CP {[:Navigation "Land"] 2
                           :Cartography 2
                           :Camouflage 2
                           :Interrogation 2
                           [:AreaKnowledge "some strategic frontier"] 1
                           [:Savoir-Faire "Military"] 1}})

(def lens-guide {:name "Guide"
                 :profession "Guide"
                 :used-CP {[:Navigation "Land"] 2
                           :Leadership 2
                           :Diplomacy 2
                           :Tactics 1
                           :Teamster 2
                           :Administration 1}})

(def lens-brigand {:name "Brigand"
                   :profession "Brigand"
                   :used-CP {:Tactics 2
                             :Staff 2
                             :Bow 2
                             :Intimidation 1
                             :Camouflage 2
                             :Disguise 1}})

(def ranger {:name "Ranger"
             :profession "Ranger"
             :used-CP {:Stealth 2
                       :FirstAid 1
                       :Hiking 2
                       :Survival 1
                       :Tracking 2
                       [:Navigation "Land"] 1
                       :AreaKnowledge 1
                       :Bow 2
                       :AxeMace 1
                       [:Mimicry "animal & bird noises"] 1}
             :optional-CP [[{:DX 20}
                            {:IQ 20}
                            {:CombatReflexes 5
                             :Per 5}]
                           [{:Per 5}
                            {:AnimalEmpathy 5}
                            {:Fit 5}]
                           (into [{:CodeofHonorPirate's -5}
                                  {:Enemy -5}]
                                 (get (:optional-CP wilderness) 0))
                           (allocate-same-to-each 2 3 #{:Camouflage
                                                        [:Riding "Equines"]
                                                        :HerbLore
                                                        [:Boating "Unpowered"]
                                                        :Fishing
                                                        :AnimalHandling
                                                        :WeatherSense
                                                        [:Fast-Draw "Arrow"]
                                                        :Net
                                                        :Naturalist})]
             :lens [lens-hunter lens-scout lens-guide lens-brigand]})

(def mage {:name "Mage"
           :profession "Mage"
           :used-CP {:Magery 15
                     :IQ 20
                     :Thaumatology 2
                     :Research 1
                     [:CurrentAffairs "Learned"] 1
                     [:AreaKnowledge "The Capital"] 1}
           :optional-CP [[{:Magery 10}
                          {:MaxFP 9
                           [:Perk "Mana Trance"] 1}
                          {:IQ 20
                           :ST -10}]
                         [{:Clueless -10}
                          {:Duty -10}
                          {:LowPainThreshold -10}
                          {:Pyromania -10}
                          {:Impulsiveness -10}
                          {:Overconfidence -10}
                          {}]
                         (allocate-points 2 [:Throwing
                                             :Staff
                                             :InnateAttack])
                         (allocate-points 3 [:Diplomacy
                                             [:Savoir-Faire "High Society"]
                                             [:History "Kingdom"]
                                             :Cartography
                                             :Linguistics
                                             [:RitualMagic "Elementalism"]
                                             [:HiddenLore "Nature Spirits"]
                                             [:HiddenLore "Demons"]])
                         (allocate-same-to-each 1 5 #{:DetectMagic
                                                      :IgniteFire
                                                      :PurifyAir
                                                      :SeekWater
                                                      :SeekEarth
                                                      :Block
                                                      :Counterspell
                                                      :Scryguard
                                                      :Apportation
                                                      :Sound})]
           :notes ["Select one [3 CP] from: Powerstone with capacity 3, Book with 10 spells, Three spells"]})

(def lens-healer {:name "Healer"
                  :profession "Healer"
                  :used-CP {:Surgery 2
                            :HerbLore 2
                            :StopBleeding 1
                            :MinorHealing 2
                            :MajorHealing 2
                            :Restoration 1}})

(def lens-inquisitor {:name "Inquisitor"
                      :profession "Inquisitor"
                      :used-CP {:DetectLies 2
                                :Interrogation 2
                                :Intimidation 1
                                [:HiddenLore "Demons"] 1
                                :Search 2
                                :SenseSpirit 1
                                :SenseHeretics 1}})

(def lens-holy-warrior {:name "Holy Warrior"
                        :profession "Holy Warrior"
                        :used-CP {[:Riding "Equines"] 1
                                  :Tactics 1
                                  :Broadsword 3
                                  :Shield 1
                                  :Flash 2
                                  :Sunbolt 1
                                  :ContinualLight 1}})

(def lens-exorcist {:name "Exorcist"
                    :profession "Exorcist"
                    :used-CP {:Exorcism 2
                              :FinalRest 1
                              :SenseSpirit 1
                              :TurnSpirit 2
                              :TurnZombie 2
                              :RepelSpirits 1
                              :Consecrate 1}})

(def sun-templar {:name "Sun Templar"
                  :profession "Sun Templar"
                  :used-CP {[:ClericalInvestment "Sun Templars"] 5
                            :PowerInvestiture 10
                            :IQ 20
                            [:Oath "Sun Templars"] -10
                            :FirstAid 2
                            :Hiking 1
                            :Shield 1
                            :Broadsword 1
                            [:ReligiousRitual "Sun God"] 1
                            :Heraldry 1
                            [:Riding "Equines"] 1
                            :PublicSpeaking 1}
                  :notes ["Select 3 different spells randomly with 2d: 1-3 Awaken, 4-5 Sense Sun, 6, Warmth, 7 Light, 8 Stop Bleeding, 9 Bright Vision, 10 Final Rest, 11-12 SenseSpirit"
                          "Select 3 spells from Sun Templar Spells"]
                  :lens [lens-healer lens-inquisitor lens-holy-warrior lens-exorcist]})

(def professions {:Soldier soldier
                  :Thief thief
                  :Ranger ranger})


;; name lists, mostly to be used with random-element
;; some are a courtesy of David Haguenauer
;; some were taken / inspired by robotfindskitten
;;  -> http://www.robotfindskitten.org/
;; (so yeah, that probably means this code is under the GPL now)

(define character-names ;; TODO or just use "you" and put these as objects ?
  '("Bob"
    "a serial killer"
    "a squatter"
    "an immolated hobo"
    "a bimbette"
    "a hipster"
    "a nymphette"
    "a wheelbo"
    "a yuppie"
    "an angry dyke"
    "a dead hooker"
    "a Malaysian transsexual"
    "a smurf"
    "a truckload of gullible kids"
    "an air marshal"
    "an archbishop"
    "James Bergstra"
    "Kris Kringle"
    "Olivia Newton-John"
    "David Bowie"
    "Santa Claus"
    "a dark-haired Argentinian bedmate"
    "Rocky Balboa"
    "Lindy Hop"
    "Mr. T."
    "Richard Nixon's nose"
    "Lucy Ricardo"
    "Gary Busey"
    "a robot"
    "Leonard Richardson"
    "John Wayne Gacy"
    "Bozo the clown"
    "your State Farm Insurance(tm) representative"
    "Stanley"
    "Carlos Tarango"
    "Pat Smear"
    "a tribe of cannibals"
    "farmer Joe"
    "a team of arctic explorers"
    "the spectre of Sherlock Holmes"
    "a mathematician"
    "a Turing machine"
    "Tigerbot Hesh"
    "the ghost of your dance instructor"
    "Fonzie"
    "Conan O'Brian, sans jawbone"
    "the ASCII Floating Head of Seth David Schoen"
    "the author of \"Randomness and Mathematical Proof\""))

(define object-names
  '("the sacred windshield of Ooga-Booga"
    "a coccyx"
    "an artificial hip"
    "a CPU fan"
    "a floating point register"
    "a priviledged instruction"
    "a stack pointer"
    "an illegal opcode"
    "the French Connection"
    "ass rape"
    "the Tylenol murders"
    "delirium tremens"
    "drugs and alcohol"
    "hypnagogic hallucinations"
    "video games"
    "termination proofs"
    "the pigeonhole principle"
    "the Protocols of the Elders of Zion"
    "the G spot"
    "an intermammary cleft"
    "a love triangle"
    "cleavage"
    "a pedosmile"
    "strap-on dildoes"
    "a good digital rectal massage"
    "bodily fluids"
    "Jerusalem artichokes"
    "tomatoes"
    "a shiny zucchini"
    "apples and bananas"
    "rutabagas"
    "a deep fried baby"
    "a mind-numbingly stupid roommate"
    "internet porn"
    "a Doomsday device"
    "colonoscopies"
    "a handbra"
    "pop quizzes"
    "the Lesbian Sex Mafia"
    "brass knuckles"
    "banal hedonism"
    "the United Negro Pizza Fund"
    "inmates"
    "spruce beer"
    "garbled time"
    "heterochromia"
    "a floozie"
    "cat food"
    "Lychrel numbers"
    "a human toilet"
    "a Cleveland steamer"
    "virtual accents"
    "taxicab geometry"
    "diplomatic intercourse"
    "a pit stop"
    "drying scum"
    "nocturnal penile tumescence"
    "an old tin can"
    "an altar to the horse god"
    "a box of dancing mechanical pencils (They dance! They sing!)"
    "an old Duke Ellington record"
    "a box of fumigation pellets"
    "a digital clock that's stuck at 2:17 PM"
    "a charred human corpse"
    "not kitten"
    "an empty shopping bag"
    "a big ugly bowling trophy"
    "a coat hanger"
    "a packet of Kool-Aid(tm)"
    "a freshly-baked pumpkin pie"
    "a lone, forgotten comma"
    "one hundred thousand carpet fibers"
    "Bill Gates' stand-up act"
    "an autographed copy of the Kama Sutra"
    "a stupid mask, fashioned after a beagle"
    "seven 1/4\" screws and a piece of plastic"
    "a 80286 machine"
    "one of those stupid \"Homes of the Stars\" maps"
    "a signpost saying \"TO KITTEN\" that points in no particular direction"
    "a hammock stretched between a tree and a volleyball pole"
    "a Texas Instruments of Destruction calculator"
    "a dark, amphorous blob of matter"
    "a pincushion"
    "a mighty zombie talking about some love and prosperity"
    "our 10 MILLION DOLLAR prize"
    "an object"
    "a mere collection of pixels"
    "a badly dented high-hat cymbal"
    "a marijuana brownie"
    "a plush Chewbacca"
    "daily hunger conditioner from Australasia"
    "some stuff"
    "a glorious fan of peacock feathers"
    "some compromising photos of Babar the Elephant"
    "a chambered nautilus"
    "a copy of the Weekly World News"
    "the proverbial wet blanket"
    "a \"Get Out of Jail Free\" card"
    "an incredibly expensive \"Mad About You\" collector plate"
    "Paul Moyer's necktie"
    "a haircut and a real job"
    "an automated robot-hater"
    "an automated robot-liker"
    "a black hole"
    "a big brick wall"
    "Heart of Darkness brand pistachio nuts"
    "a smoking branding iron shaped like a 24-pin connector"
    "a Java applet"
    "a can of Spam Lite"
    "a Mentos wrapper"
    "a fly on the wall"
    "a spindle, and a grindle, and a bucka-wacka-woom"
    "a toenail"
    "the fish"
    "a Buttertonsils bar"
    "the uniform of a Revolutionary-era minuteman"
    "a punch bowl"
    "a perpetual immobility machine"
    "an optical illusion"
    "the World's Biggest Motzah Ball"
    "a rather large stack of trashy romance novels"
    "exclamation points!"
    "a herd of wild coffee mugs"
    "a limbo bar"
    "a vase full of artificial flowers"
    "a large snake"
    "a pair of saloon-style doors"
    "a bust of Beethoven"
    "the amazing self-referential thing"
    "a feather boa"
    "topsoil"
    "a large brown bear"
    "an inverted billiard ball"
    "the intermission from a 1930s silent movie"
    "a man selling an albatross"
    "a piece of cloth used to cover a stage in between performances"
    "a U.S. president"
    "a Cat 5 cable"
    "a number of short theatrical productions"
    "a canister of pressurized whipped cream, sans whipped cream"
    "stimutacs"
    "the fountain of youth"
    "a book: Feng Shui, Zen: the art of randomly arranging items"
    "a bag of groceries"
    "a mirror"
    "a bitchin' homemade tesla coil"
    "a frosted pink party-cake, half eaten"
    "a bottle of nail polish remover"
    "the triangle leg adjacent to an angle divided by the leg opposite it"
    "the crusty exoskeleton of an arthropod"))

(define dungeon-names ;; TODO too few, take some of the objects
  '("the maze of Doom"
    "the planet Mars"
    "the favelas"
    "the shanty towns"
    "the slums"
    "the Voronoi diagram"
    "the basement"
    "a dark washroom"
    "the closet"
    "a dark alley"
    "the Norwegian embassy"
    "the departement store"
    "inside a crocodile"
    "the Will Rogers Highway"
    "the local draft board"
    "a fine mess"
    "the scenery for \"Waiting for Godot\""
    "an abandoned used-car lot"
    "a grain elevator"
    "the constellation Pisces"
    "a helicopter"
    "a patch of mushrooms"
    "a patch of grape jelly"
    "one of the few remaining discoes"
    "a boating accident"
    "a livery stable"
    "the horizon"
    "the ionosphere"
    "a brownstone"
    "a technical university in Australia"
    "Emporer Shaddam the 4th's planet"))

;;   "This object here appears to be Louis Farrakhan's bow tie.",
;;   "This is the world-famous Chain of Jockstraps.",
;;   "A trash compactor, compacting away.",
;;   "This toaster strudel is riddled with bullet holes!",
;;   "It's a hologram of a crashed helicopter.",
;;   "This is a television. On screen you see a robot strangely similar to yourself.",
;;   "This balogna has a first name, it's R-A-N-C-I-D.",
;;   "A salmon hatchery? Look again. It's merely a single salmon.",
;;   "It's a rim shot. Ba-da-boom!",
;;   "It's creepy and it's kooky, mysterious and spooky. It's also somewhat ooky.",
;;   "This is an anagram.",
;;   "This object is like an analogy.",
;;   "It's a symbol. You see in it a model for all symbols everywhere.",
;;   "The object pushes back at you.",
;;   "A traffic signal. It appears to have been recently vandalized.",
;;   "\"There is no kitten!\" cackles the old crone. You are shocked by her blasphemy.",
;;   "This is a Lagrange point. Don't come too close now.",
;;   "The dirty old tramp bemoans the loss of his harmonica.",
;;   "Look, it's Fanny the Irishman!",
;;   "What in blazes is this?",
;;   "It's the instruction manual for a previous version of this game.",
;;   "A brain cell. Oddly enough, it seems to be functioning.",
;;   "Tea and/or crumpets.",
;;   "This jukebox has nothing but Cliff Richards albums in it.",
;;   "It's a Quaker Oatmeal tube, converted into a drum.",
;;   "This is a remote control. Being a robot, you keep a wide berth.",
;;   "It's a roll of industrial-strength copper wire.",
;;   "Oh boy! Grub! Er, grubs.",
;;   "A puddle of mud, where the mudskippers play.",
;;   "Plenty of nothing.",
;;   "Look at that, it's the Crudmobile.",
;;   "Just Walter Mattheau and Jack Lemmon.",
;;   "Two crepes, two crepes in a box.",
;;   "An autographed copy of \"Primary Colors\", by Anonymous.",
;;   "Another rabbit? That's three today!",
;;   "It's a segmentation fault. Core dumped, by the way.",
;;   "A historical marker showing the actual location of /dev/null.",
;;   "Thar's Mobius Dick, the convoluted whale. Arrr!",
;;   "It's a charcoal briquette, smoking away.",
;;   "A pizza, melting in the sun.",
;;   "It's a \"HOME ALONE 2: Lost in New York\" novelty cup.",
;;   "A stack of 7 inch floppies wobbles precariously.",
;;   "It's nothing but a corrupted floppy. Coaster anyone?",
;;   "A section of glowing phosphor cells sings a song of radiation to you.",
;;   "This TRS-80 III is eerily silent.",
;;   "A toilet bowl occupies this space.",
;;   "This peg-leg is stuck in a knothole!",
;;   "It's a solitary vacuum tube.",
;;   "This corroded robot is clutching a mitten.",
;;   "\"Hi, I'm Anson Williams, TV's 'Potsy'.\"",
;;   "This subwoofer was blown out in 1974.",
;;   "Three half-pennies and a wooden nickel.",
;;   "It's the missing chapter to \"A Clockwork Orange\".",
;;   "It's a burrito stand flyer. \"Taqueria El Ranchito\".",
;;   "This smiling family is happy because they eat LARD.",
;;   "Roger Avery, persona un famoso de los Estados Unidos.",
;;   "Ne'er but a potted plant.",
;;   "A parrot, kipping on its back.",
;;   "A forgotten telephone switchboard.",
;;   "A forgotten telephone switchboard operator.",
;;   "It's an automated robot-disdainer. It pretends you're not there.",
;;   "It's a portable hole. A sign reads: \"Closed for the winter\".",
;;   "Just a moldy loaf of bread.",
;;   "A little glass tub of Carmex. ($.89) Too bad you have no lips.",
;;   "A Swiss-Army knife. All of its appendages are out. (toothpick lost)",
;;   "It's a zen simulation, trapped within an ASCII character.",
;;   "It's a copy of \"The Rubaiyat of Spike Schudy\".",
;;   "It's \"War and Peace\" (unabridged, very small print).",
;;   "A willing, ripe tomato bemoans your inability to digest fruit.",
;;   "A robot comedian. You feel amused.",
;;   "It's KITT, the talking car.",
;;   "Here's Pete Peterson. His batteries seem to have long gone dead.",
;;   "\"Blup, blup, blup\", says the mud pot.",
;;   "More grist for the mill.",
;;   "Grind 'em up, spit 'em out, they're twigs.",
;;   "The boom box cranks out an old Ethel Merman tune.",
;;   "It's \"Finding kitten\", published by O'Reilly and Associates.",
;;   "Pumpkin pie spice.",
;;   "It's the Bass-Matic '76! Mmm, that's good bass!",
;;   "\"Lend us a fiver 'til Thursday\", pleas Andy Capp.",
;;   "It's a tape of '70s rock. All original hits! All original artists!",
;;   "You've found the fabled America Online disk graveyard!",
;;   "Empty jewelboxes litter the landscape.",
;;   "It's the astounding meta-object.",
;;   "Ed McMahon stands here, lost in thought. Seeing you, he bellows, \"YES SIR!\"",
;;   "...thingy???",
;;   "It's 1000 secrets the government doesn't want you to know!",
;;   "The letters O and R.",
;;   "A magical... magic thing.",
;;   "It's a moment of silence.",
;;   "It's Sirhan-Sirhan, looking guilty.",
;;   "It's \"Chicken Soup for the Kitten-seeking Soulless Robot.\"",
;;   "It is a set of wind-up chatter teeth.",
;;   "It is a cloud shaped like an ox.",
;;   "You see a snowflake here, melting slowly.",
;;   "It's a big block of ice. Something seems to be frozen inside it.",
;;   "Vladimir Lenin's casket rests here.",
;;   "It's a copy of \"Zen and The Art of Robot Maintenance\".",
;;   "This invisible box contains a pantomime horse.",
;;   "A mason jar lies here open. It's label reads: \"do not open!\".",
;;   "A train of thought chugs through here.",
;;   "This jar of pickles expired in 1957.",
;;   "Someone's identity disk lies here.",
;;   "\"Yes!\" says the bit.",
;;   "\"No!\" says the bit.",
;;   "A dodecahedron bars your way.",
;;   "Mr. Hooper is here, surfing.",
;;   "It's a big smoking fish.",
;;   "You have new mail in /var/spool/robot",
;;   "Just a monitor with the blue element burnt out.",
;;   "A pile of coaxial plumbing lies here.",
;;   "It's a rotten old shoe.",
;;   "It's a hundred-dollar bill.",
;;   "It's a Dvorak keyboard.",
;;   "It's a cardboard box full of 8-tracks.",
;;   "Just a broken hard drive containg the archives of Nerth Pork.",
;;   "A broken metronome sits here, it's needle off to one side.",
;;   "A sign reads: \"Go home!\"",
;;   "A sign reads: \"No robots allowed!\"",
;;   "It's the handheld robotfindskitten game, by Tiger.",
;;   "This particular monstrosity appears to be ENIAC.",
;;   "This is a tasty-looking banana creme pie.",
;;   "A wireframe model of a hot dog rotates in space here.",
;;   "Just the empty husk of a locust.",
;;   "You disturb a murder of crows.",
;;   "It's a copy of the robotfindskitten EULA.",
;;   "It's Death.",
;;   "It's an autographed copy of \"Secondary Colors,\" by Bob Ross.",
;;   "It is a marzipan dreadnought that appears to have melted and stuck.",
;;   "It's a DVD of \"Crouching Monkey, Hidden Kitten\", region encoded for the moon.",
;;   "It's Kieran Hervold.  Damn dyslexia!",
;;   "A non-descript box of crackers.",
;;   "Carbonated Water, High Fructose Corn Syrup, Color, Phosphoric Acid, Flavors, Caffeine.",
;;   "\"Move along! Nothing to see here!\"",
;;   "It's the embalmed corpse of Vladimir Lenin.",
;;   "A coupon for one free steak-fish at your local family diner.",
;;   "A set of keys to a 2001 Rolls Royce. Worthless.",
;;   "A gravestone stands here.  \"Izchak Miller, ascended.\"",
;;   "Someone has written \"ad aerarium\" on the ground here.",
;;   "A large blue eye floats in midair.",
;;   "This appears to be a statue of Perseus.",
;;   "There is an opulent throne here.",
;;   "It's a squad of Keystone Kops.",
;;   "This seems to be junk mail addressed to the finder of the Eye of Larn.",
;;   "A wondrous and intricate golden amulet.  Too bad you have no neck.",
;;   "The swampy ground around you seems to stink with disease.",
;;   "An animate blob of acid.  Being metallic, you keep well away.",
;;   "It's a copy of Knuth with the chapter on kitten-search algorithms torn out.",
;;   "A crowd of people, and at the center, a popular misconception.",
;;   "It's a blind man. When you touch, he exclaims \"It's a kitten prospecting robot!\"",
;;   "It's a lost wallet. It's owner didn't have pets, so you discard it.",
;;   "This place is called Antarctica. There is no kitten here.",
;;   "It's a mousetrap, baited with soap.",
;;   "A book with \"Don't Panic\" in large friendly letters across the cover.",
;;   "A compendium of haiku about metals.",
;;   "A discredited cosmology, relic of a bygone era.",
;;   "A hollow voice says \"Plugh\".",
;;   "A knight who says \"Either I am an insane knave, or you will find kitten.\"",     
;;   "A neural net -- maybe it's trying to recognize kitten.",
;;   "A screwdriver.",
;;   "A statue of a girl holding a goose like the one in Gottingen, Germany.",
;;   "A tetradrachm dated \"42 B.C.\"",
;;   "A voice booms out \"Onward, kitten soldiers...\"",
;;   "An eminently forgettable zahir.",
;;   "Apparently, it's Edmund Burke.",
;;   "For a moment, you feel something in your hands, but it disappears!",
;;   "Here is a book about Robert Kennedy.",
;;   "Hey, robot, leave those lists alone.",
;;   "Ho hum.  Another synthetic a posteriori.",
;;   "It's Asimov's Laws of Robotics.  You feel a strange affinity for them.",
;;   "It's Bach's Mass in B-minor!",
;;   "It's a bug.",
;;   "It's a synthetic a priori truth!  Immanuel would be so pleased!",
;;   "It's the Tiki Room.",
;;   "Just some old play by a Czech playwright, and you can't read Czech.",
;;   "Kitten is the letter 'Q'.  Oh, wait, maybe not.",
;;   "Quidquid Latine dictum sit, kitten non est.",
;;   "Sutro Tower is visible at some distance through the fog.",
;;   "The Digital Millennium Copyright Act of 1998.",
;;   "The United States Court of Appeals for the Federal Circuit.",
;;   "The non-kitten item like this but with \"false\" and \"true\" switched is true.", 
;;   "The non-kitten item like this but with \"true\" and \"false\" switched is false.",
;;   "This is the chapter called \"A Map of the Cat?\" from Feynman's autobiography.",  
;;   "This is the forest primeval.",
;;   "Werner's \"Pocket Field Guide to Things That Are Not Kitten\".",
;;   "You found nettik, but that's backwards.",
;;   "You have found some zinc, but you must not stop here, for you must find kitten.", 
;;   "\"50 Years Among the Non-Kitten Items\", by Ann Droyd.",
;;   "\"Robot may not injure kitten, or, through inaction, ...\"",
;;   "\"Address Allocation for Private Internets\" by Yakov Rekhter et al.",
;;   "\"Mail Routing and the Domain System\" by Craig Partridge.",
;;   "\"The Theory and Practice of Oligarchical Collectivism\" by Emmanuel Goldstein.", 
;;   "\"201 Kitten Verbs, Fully Conjugated\".  You look for \"find\".",
;;   "A card shark sits here, practicing his Faro shuffle.  He ignores you.",
;;   "A copy of DeCSS.  They're a dime a dozen these days.",
;;   "A demonic voice proclaims \"There is no kitten, only Zuul\".  You flee.",
;;   "A lotus.  You make an interesting pair.",
;;   "A milk carton, with a black and white picture of kitten on the side.",
;;   "Any ordinary robot could see from a mile away that this wasn't kitten.",
;;   "A stegosaurus, escaped from the stegosaurusfindsrobot game.  It finds you.",
;;   "Baling wire and chewing gum.",
;;   "Chewing gum and baling wire.",
;;   "Here is no kitten but only rock, rock and no kitten and the sandy road.",
;;   "Hey, I bet you thought this was kitten.",
;;   "It is an ancient mariner, and he stoppeth one of three.",
;;   "It pleases you to be kind to what appears to be kitten -- but it's not!",
;;   "It's a blatant plug for Ogg Vorbis, http://www.vorbis.com/",
;;   "It's a business plan for a new startup, kitten.net.",
;;   "It's a revised business plan for a new startup, my.kitten.net.",
;;   "It's a square.",
;;   "It seems to be a copy of \"A Tail of Two Kitties\".",
;;   "It's the Donation of Constantine!",
;;   "It's this message, nothing more.",
;;   "Lysine, an essential amino acid.  Well, maybe not for robots.",
;;   "No kitten here.",
;;   "The score for a Czech composer's \"Kitten-Finding Symphony in C\".",
;;   "This looks like Bradley's \"Appearance and Reality\", but it's really not.",
;;   "This non-kitten item no verb.",
;;   "You feel strangely unfulfilled.",
;;   "You hit the non-kitten item.  The non-kitten item fails to yowl.",
;;   "You suddenly yearn for your distant homeland.",
;;   "You've found the snows of yesteryear!  So that's where they all went to.",
;;   "Approaching.  One car.  J.  Followed by.  Two car.  M, M.  In five. Minutes.",
;;   "Free Jon Johansen!",
;;   "Free Dmitry Sklyarov!",
;;   "One person shouts \"What do we want?\" The crowd answers \"Free Dmitry!\"",
;;   "Judith Platt insults librarians.",
;;   "This map is not the territory.",
;;   "\"Go back to Libraria!\", says Pat Schroeder.",
;;   "This is a porcelain kitten-counter.  0, 0, 0, 0, 0...",
;;   "An old bootable business card, unfortunately cracked down the middle.",
;;   "A kitten sink, for washing kitten (if only kitten liked water).",
;;   "A kitten source (to match the kitten sink).",
;;   "If it's one thing, it's not another.",
;;   "If it's not one thing, it's another.",
;;   "A caboodle.",
;;   "A grin.",
;;   "A hedgehog.  It looks like it knows something important.",
;;   "You've found... Oh wait, that's just a cat.",
;;   "Robot should not be touching that.",
;;   "Air Guitar!!!  NA na NA na!!",
;;   "An aromatherapy candle burns with healing light.",
;;   "You find a bright shiny penny.",
;;   "It's a free Jon Johansen!",
;;   "It's a free Dmitry Sklyarov!",
;;   "The rothe hits!  The rothe hits!",
;;   "It's an Internet chain letter about sodium laureth sulfate.",
;;   "Ed Witten sits here, pondering string theory.",
;;   "Something is written here in the dust.  You read: \"rJbotf ndQkttten\".",
;;   "We wish you a merry kitten, and a happy New Year!",
;;   "Run away!  Run away!",
;;   "You can see right through this copy of Brin\'s \"Transparent Society\".",
;;   "This copy of \"Steal This Book\" has been stolen from a bookstore.",
;;   "It's Roya Naini.",
;;   "This kit is the fourteenth in a series of kits named with Roman letters.",
;;   "This is the tenth key you've found so far.",
;;   "You find a fraud scheme in which loans are used as security for other loans.",
;;   "It's the phrase \"and her\", written in ancient Greek.",

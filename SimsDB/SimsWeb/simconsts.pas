unit simconsts;

{$mode ObjFPC}{$H+}

interface

const
  SIM_LIFE_STAGES: Array of String = (
      'Baby',
      'Toddler',
      'Child',
      'Teen',
      'Young Adult',
      'Adult',
      'Elder',
      'Dead');

  SIM_ZODIAC_SIGNS: Array of String = (
      'Aries',
      'Taurus',
      'Gemini',
      'Cancer',
      'Leo',
      'Virgo',
      'Libra',
      'Scorpio',
      'Sagittarius',
      'Capricorn',
      'Aquarius',
      'Pisces');

  SIM_PERSONALITY: Array of String = ('Neat', 'Outgoing', 'Active', 'Playful',
                                      'Nice');

  SIM_SKILLS: Array of String = ('Cooking', 'Mechanical', 'Charisma', 'Body',
                                 'Logic', 'Creativity');

implementation

end.


/*
    This file defines all structs needed in behind_the_edge

    Parts of this file are part of SWI-Prolog
    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1999-2011, University of Amsterdam
    All rights reserved.
    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:
    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.
    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef _BEHIND_THE_CORNER_SCENE_H_INCLUDED
#define _BEHIND_THE_CORNER_SCENE_H_INCLUDED

#define BEHIND_THE_CORNER_VERSION "0.1.0"
#include <SWI-Prolog.h>
#include <vector>
#include <tuple>
#include <string>
#ifdef __WINDOWS__
#include <windows.h>
#endif
#include "abType.h"
#include "utils.h"
#include <boost/qvm/vec.hpp>

#define SCENE_VALUE_MAGIC 17823491

// typedef struct fieldtag
// {
//   int x_normal;
//   int y_normal;
//   int ab_type;
// } field, *Field;

// typedef struct scenetag
// {
//   int magic;                /* TABLE_MAGIC */
//   char* file;              /* name of the file */
//   int width;              /* # width of the scene */
//   int height;             /* height of the scene */
//   // Field fields;             /* field description terms */
//   int keyfield;             /* 0-based index of key (or -1) */
//   Field *values;             /* pointer to actual scene values */
//   bool loaded;
// #ifdef __WINDOWS__
//   HANDLE hfile; /* handle to the file */
//   HANDLE hmap;  /* handle to the map */
// #endif
// #ifdef HAVE_MMAP
//   int fd; /* file descriptor */
// #endif
// } scene, *Scene;

class Zone
{
public:
  boost::qvm::vec<float, 2> normal;
  ABType ab_type;
  Zone() : normal({0,0}), ab_type(Background) {}
  Zone(float x_normal, float y_normal, ABType ab_type) : normal({x_normal,y_normal}), ab_type(ab_type) {}
  ~Zone();
  float getX();
  float getY();
};

class Scene
{
private:
  std::vector<std::vector<Zone>> values; /* pointer to actual scene values */

public:
  int magic;  /* TABLE_MAGIC */
  std::string file; /* name of the file */
  int width;  /* width of the scene */
  int height; /* height of the scene */
  int edge_length;
  // Field fields;             /* field description terms */
  bool loaded;
  Scene() {}
  Scene(std::string filename): file(filename), loaded(false), magic(SCENE_VALUE_MAGIC){}
  ~Scene();
  int load();
  int loadFromScene(const Scene &scene, int edge_length);
  const Zone &getZone(int x, int y) const;
  const Zone &getZone(Coords coords) const;
  boost::qvm::vec<float, 2> getZoneVector(int x, int y) const;
  boost::qvm::vec<float, 2> getZoneVector(Coords coords) const;
  Zone getAverageZone(int x, int y, int w, int h) const;
};

#endif
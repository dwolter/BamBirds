#include <iostream>
#include <cmath>
#include <string>
#include <fstream>
#include <vector>
#include <map>
#include <unordered_map>
#include <boost/algorithm/string.hpp>
#include "abType.h"
#include "scene.h"
#include "utils.h"
#include <boost/qvm/vec.hpp>
#include <boost/qvm/vec_operations.hpp>
#include <boost/qvm/vec_access.hpp>
#include <chrono>
using namespace std;
using namespace boost::qvm;

Zone::~Zone()
{
}

float Zone::getX()
{
  return A0(normal);
}

float Zone::getY()
{
  return A1(normal);
}

Zone groundZone = Zone(0,1,Ground);
Zone airZone = Zone(0,0,Background);

Scene::~Scene()
{
  // cout << "Scene deconstructor of " << file << " called" << endl;
}

int Scene::load()
{
  // auto start = chrono::steady_clock::now();
  vector<int32_t> file_values;
  long valuelength;
  int32_t width = 0;
  int32_t height = 0;
  fstream newfile;
  newfile.open(this->file, ios::in);
  if (newfile.is_open())
  { //checking whether the file is open
    string first;
    getline(newfile, first);
    vector<string> widthAndHeight;
    boost::split(widthAndHeight, first, [](char c) { return c == ','; });
    width = stoi(widthAndHeight.at(0));
    height = stoi(widthAndHeight.at(1));
    // cout << widthAndHeight.at(1) << endl;
    file_values.reserve(width*height*3);
    string tp;
    while (getline(newfile, tp))
    { //read data from file object and put it into string.
      vector<string> results;
      boost::split(results, tp, [](char c) { return c == ','; });
      for (auto &elem : results)
      {
        if (elem.length() == 0)
        {
          break;
        }
        file_values.push_back(stoi(elem));
      }
    }
    newfile.close(); //close the file object.
  }
  else
  {
    cerr << "Failed to load scene from " << file << endl;
    return FALSE;
  }

  this->values.resize(width, vector<Zone>(height));
  this->width = width;
  this->height = height;
  // cout << "Scene:" << endl;
  // cout << " width: " << this->width << endl;
  // cout << " height: " << this->height << endl;
  // cout << " file values length: " << file_values.size() << endl;

  int count_non_background = 0;

  for (int x = 0; x < width; x ++)
  {
    for (int y = 0; y < height; y ++)
    {
      Zone zone = this->values[x][y];
      int index = x + (height-1-y) * width;
      this->values[x][y].ab_type = (ABType)file_values.at(index);
      if (this->values[x][y].ab_type != Background)
        count_non_background++;
      //TODO: Normalize vector
      vec<float, 2> vector = {
        (float) file_values.at(index + width * height), 
        (float) file_values.at(index + 2 * width * height)};
      if (A0(vector) != 0 || A1(vector) != 0)
      {
        A0(vector) -= 127;
        A1(vector) -= 127;
        A1(vector) = -A1(vector);
        
        if (mag_sqr(vector) != 0)
          normalize(vector);
      }
      this->values[x][y].normal = vector;
    }
  }
  // auto end = chrono::steady_clock::now();
  // auto diff = end - start;
  // cout << "done loading scene in " << chrono::duration <double, milli> (diff).count() << " ms" << endl;
  // cout << "Count of non_background: " << count_non_background << endl;
  this->loaded = true;
  return TRUE;
}

int Scene::loadFromScene(const Scene &scene, int edge_length)
{
  // auto start = chrono::steady_clock::now();
  this->edge_length = edge_length;
  this->file = scene.file + " edge_length " + to_string(edge_length);
  this->width = ceil((float) scene.width/edge_length);
  this->height = ceil((float) scene.height/edge_length);
  this->values.resize(width, vector<Zone>(height));
  // cout << "Scene:" << endl;
  // cout << " width: " << this->width << endl;
  // cout << " height: " << this->height << endl;
  // int count_non_background = 0;
  for (int x = 0; x < width; x++)
  {
    for (int y = 0; y < height; y ++)
    {
      values[x][y] = scene.getAverageZone(x*edge_length,y*edge_length,edge_length, edge_length);
      // if (values[x][y].ab_type != Background)
      // {
      //   count_non_background++;
      // }
    }
  }
  loaded = true;
  // auto end = chrono::steady_clock::now();
  // auto diff = end - start;
  // cout << "shrinking scene done in " << chrono::duration <double, milli> (diff).count() << " ms" << endl;
  // cout << "Count of non_background: " << count_non_background << endl;
  return TRUE;
}

template<typename A, typename B>
std::pair<B,A> flip_pair(const std::pair<A,B> &p)
{
    return std::pair<B,A>(p.second, p.first);
}

template<typename A, typename B>
std::multimap<B,A> flip_map(const std::map<A,B> &src)
{
    std::multimap<B,A> dst;
    std::transform(src.begin(), src.end(), std::inserter(dst, dst.begin()), 
                   flip_pair<A,B>);
    return dst;
}

template<typename A, typename B>
std::multimap<B,A> flip_unordered_map(const std::unordered_map<A,B> &src)
{
    std::multimap<B,A> dst;
    std::transform(src.begin(), src.end(), std::inserter(dst, dst.begin()), 
                   flip_pair<A,B>);
    return dst;
}

template <typename T, typename A>
int arg_max(std::vector<T, A> const& vec) {
  return static_cast<int>(std::distance(vec.begin(), max_element(vec.begin(), vec.end())));
}

template <typename T, typename A>
int arg_min(std::vector<T, A> const& vec) {
  return static_cast<int>(std::distance(vec.begin(), min_element(vec.begin(), vec.end())));
}

Zone Scene::getAverageZone(int x, int y, int w, int h) const
{
  map<ABType, int> ab_type_counts;
  
  vector<vec<float,2>> unique_normals;
  vector<float> normals_count;
  for (int i = x; i < min(x+w, width); i++)
  {
    for (int j = y; j < min(y+h, height); j++)
    {
      Zone zone = getZone(i,j);
      ab_type_counts[zone.ab_type] += 1;
      bool found = false;
      for (auto vec_it = 0; vec_it != unique_normals.size(); vec_it++)
      {
        if (unique_normals[vec_it] == zone.normal)
        {
          found = true;
          // TODO: probably not working
          normals_count[vec_it] += 1;
        }
      }
      if (!found)
      {
        unique_normals.push_back(zone.normal);
        normals_count.push_back(1);
      }
    }
  }
  // cout << "Length of normals_count: " << normals_count.size() << endl;
  // cout << "Length of unique_normal: " << unique_normals.size() << endl;
  Zone result;
  auto flipped_ab_type = flip_map(ab_type_counts);
  for (auto pair : flipped_ab_type)
  {
    if (pair.second != Background)
    {
      result.ab_type = pair.second;
      break;
    }
  }
  int max_vector = arg_max(normals_count);
  while(mag_sqr(unique_normals[max_vector]) == 0 && normals_count[max_vector] != 0)
  {
    normals_count[max_vector] = 0;
    max_vector = arg_max(normals_count);
  }
  result.normal = unique_normals[max_vector];
  return result;
}

const Zone &Scene::getZone(int x, int y) const
{
  if (y < 0) 
  {
    return groundZone;
  }
  if (y >= height)
  {
    return airZone;
  }
  return values.at(x).at(y);
}
const Zone &Scene::getZone(Coords coords) const
{
  return getZone(coords.first, coords.second);
}
vec<float, 2> Scene::getZoneVector(int x, int y) const
{
  return getZone(x,y).normal;
}
vec<float, 2> Scene::getZoneVector(Coords coords) const
{
  return getZoneVector(coords.first, coords.second);
}

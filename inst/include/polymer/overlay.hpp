// polymer/overlay.hpp: n-ary overlay labelling by propagation over a mesh.
//
// Copyright (c) 2026 Michael Sumner. MIT licence, see polymer's LICENSE.
//
// Dependency-free C++17, header only. Given a triangle mesh (counter-
// clockwise triangles, 0-based vertex ids), its constraint edges and the
// edge_sources table (one row per constraint edge and input segment
// covering it, with the segment's feature and direction), label every
// triangle with the set of features covering it under a fill rule.
//
// This is mesh spec v0.2 Annex A. The labelling is purely combinatorial:
// it uses triangle adjacency, edge ids and source lists, never
// coordinates, so it is exact whenever the noding was exact.
//
// Winding: crossing a source segment of feature f from its left to its
// right changes w[f] by -1, from right to left by +1. Outside the mesh
// w = 0 for every feature. Each connected component is seeded from a
// triangle side on the mesh boundary and walked breadth-first. Every side
// not on the breadth-first tree is checked (both of its triangles must
// agree, and a boundary side must imply w = 0 outside); the number of
// sides that disagree is `inconsistent_edges`, and a non-zero count means
// the noding is broken (a missed crossing, a dropped constraint or a
// missing source row).
//
// Winding states (the sorted (feature, w != 0) pairs) are interned, and
// transitions (state, edge, side) -> state are cached, so the cost is
// linear in triangles plus edge_sources rows, times the size of the
// states actually met. Covering sets are interned again under the fill
// rule and numbered densely in order of first appearance by triangle.

#ifndef POLYMER_OVERLAY_HPP
#define POLYMER_OVERLAY_HPP

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <stdexcept>
#include <unordered_map>
#include <utility>
#include <vector>

namespace polymer {
namespace overlay {

enum class FillRule { EvenOdd, NonZero, Positive };

// Borrowed views, all 0-based.
struct Mesh {
  const int32_t* tri = nullptr;   // 3 * n_tri vertex ids, triangle by triangle, CCW
  int64_t n_tri = 0;
  const int32_t* edge = nullptr;  // 2 * n_edge vertex ids, constraint edges v0, v1
  int64_t n_edge = 0;
};

struct EdgeSources {
  const int32_t* edge = nullptr;     // constraint edge (row of Mesh::edge)
  const int32_t* feature = nullptr;  // feature of the covering input segment
  const int8_t* forward = nullptr;   // 1 when the segment runs v0 -> v1 of its edge
  int64_t n = 0;
};

struct Labels {
  std::vector<int32_t> tri_set;     // n_tri, covering set id or -1 for none
  std::vector<int64_t> set_offset;  // n_set + 1, CSR offsets into set_feature
  std::vector<int32_t> set_feature; // features of each set, ascending
  int64_t inconsistent_edges = 0;   // sides that fail the self-check
  int64_t n_state = 0;              // distinct winding states met
};

namespace detail {

using Entry = std::pair<int32_t, int32_t>;  // (feature, winding or delta)
using Entries = std::vector<Entry>;

inline uint64_t mix(uint64_t h) {
  h ^= h >> 33;
  h *= 0xff51afd7ed558ccdULL;
  h ^= h >> 33;
  h *= 0xc4ceb9fe1a85ec53ULL;
  h ^= h >> 33;
  return h;
}

struct EntriesHash {
  std::size_t operator()(const Entries& e) const {
    uint64_t h = 0x9e3779b97f4a7c15ULL ^ e.size();
    for (const Entry& x : e) {
      h = mix(h ^ ((uint64_t)(uint32_t)x.first << 32 | (uint32_t)x.second));
    }
    return (std::size_t)h;
  }
};

struct IdsHash {
  std::size_t operator()(const std::vector<int32_t>& e) const {
    uint64_t h = 0x9e3779b97f4a7c15ULL ^ e.size();
    for (int32_t x : e) h = mix(h ^ (uint32_t)x);
    return (std::size_t)h;
  }
};

struct U64Hash {
  std::size_t operator()(uint64_t k) const { return (std::size_t)mix(k); }
};

inline uint64_t key2(int32_t a, int32_t b) {
  return (uint64_t)(uint32_t)a << 32 | (uint32_t)b;
}

// state + sign * delta, dropping zeros; both inputs sorted by feature
inline Entries step(const Entries& state, const Entries& delta, int sign) {
  Entries out;
  out.reserve(state.size() + delta.size());
  std::size_t i = 0, j = 0;
  while (i < state.size() || j < delta.size()) {
    if (j == delta.size() || (i < state.size() && state[i].first < delta[j].first)) {
      out.push_back(state[i++]);
    } else if (i == state.size() || delta[j].first < state[i].first) {
      out.emplace_back(delta[j].first, sign * delta[j].second);
      j++;
    } else {
      int32_t w = state[i].second + sign * delta[j].second;
      if (w != 0) out.emplace_back(state[i].first, w);
      i++;
      j++;
    }
  }
  return out;
}

inline bool covers(int32_t w, FillRule rule) {
  switch (rule) {
    case FillRule::EvenOdd: return (w % 2) != 0;
    case FillRule::NonZero: return w != 0;
    case FillRule::Positive: return w > 0;
  }
  return false;
}

}  // namespace detail

inline Labels label(const Mesh& mesh, const EdgeSources& sources,
                    FillRule rule = FillRule::NonZero) {
  using namespace detail;
  const int64_t nt = mesh.n_tri;
  const int64_t nh = 3 * nt;
  if (nt < 0 || mesh.n_edge < 0 || sources.n < 0) {
    throw std::invalid_argument("negative size");
  }

  // triangle sides keyed undirected (lo, hi), sorted: a side met once is
  // on the mesh boundary, twice is shared by two triangles. Triangle h / 3
  // is on the left of half-edge h, which runs tri[h] -> next vertex.
  auto from = [&](int64_t h) { return mesh.tri[h]; };
  auto to = [&](int64_t h) { return mesh.tri[3 * (h / 3) + (h + 1) % 3]; };
  std::vector<std::pair<uint64_t, int64_t>> sides((std::size_t)nh);
  for (int64_t h = 0; h < nh; h++) {
    int32_t a = from(h), b = to(h);
    if (a < 0 || b < 0) throw std::invalid_argument("negative vertex id in triangles");
    sides[(std::size_t)h] = {key2(std::min(a, b), std::max(a, b)), h};
  }
  std::sort(sides.begin(), sides.end());
  std::vector<int64_t> twin((std::size_t)nh, -1);
  for (std::size_t i = 0; i < sides.size();) {
    std::size_t j = i + 1;
    while (j < sides.size() && sides[j].first == sides[i].first) j++;
    if (j - i > 2) throw std::invalid_argument("a triangle side is shared by more than two triangles");
    if (j - i == 2) {
      int64_t g = sides[i].second, h = sides[i + 1].second;
      if (from(g) == from(h)) {
        throw std::invalid_argument("two triangles run a shared side the same way: the mesh is not consistently CCW");
      }
      twin[(std::size_t)g] = h;
      twin[(std::size_t)h] = g;
    }
    i = j;
  }

  // sources keyed by their side (lo, hi), with d = +1 when the source runs
  // lo -> hi; summed per feature these are the side's delta. Crossing a
  // side from the left of lo -> hi to its right adds -d.
  struct Src { uint64_t key; int32_t feature; int32_t d; };
  std::vector<Src> src((std::size_t)sources.n);
  for (int64_t r = 0; r < sources.n; r++) {
    int32_t e = sources.edge[r];
    if (e < 0 || e >= mesh.n_edge) throw std::invalid_argument("edge_sources edge out of range");
    int32_t v0 = mesh.edge[2 * (int64_t)e], v1 = mesh.edge[2 * (int64_t)e + 1];
    int32_t d = sources.forward[r] ? 1 : -1;
    if (v0 > v1) d = -d;
    src[(std::size_t)r] = {key2(std::min(v0, v1), std::max(v0, v1)), sources.feature[r], d};
  }
  std::sort(src.begin(), src.end(), [](const Src& x, const Src& y) {
    return x.key < y.key || (x.key == y.key && x.feature < y.feature);
  });

  // delta of each half-edge, interned by content (most sides carry one of
  // a few patterns, so transitions cache well): id, or -1 for none
  std::vector<int32_t> half_delta((std::size_t)nh, -1);
  std::vector<Entries> deltas;
  std::unordered_map<Entries, int32_t, EntriesHash> delta_id_of;
  {
    std::size_t k = 0;  // position in sides
    Entries agg;
    for (std::size_t i = 0; i < src.size();) {
      std::size_t j = i;
      agg.clear();
      while (j < src.size() && src[j].key == src[i].key) {
        if (!agg.empty() && agg.back().first == src[j].feature) {
          agg.back().second += src[j].d;
        } else {
          agg.emplace_back(src[j].feature, src[j].d);
        }
        j++;
      }
      agg.erase(std::remove_if(agg.begin(), agg.end(),
                               [](const Entry& x) { return x.second == 0; }),
                agg.end());
      uint64_t key = src[i].key;
      i = j;
      if (agg.empty()) continue;
      while (k < sides.size() && sides[k].first < key) k++;
      if (k == sides.size() || sides[k].first != key) continue;  // not a mesh side
      auto ins = delta_id_of.emplace(agg, (int32_t)deltas.size());
      if (ins.second) deltas.push_back(agg);
      for (; k < sides.size() && sides[k].first == key; k++) {
        half_delta[(std::size_t)sides[k].second] = ins.first->second;
      }
    }
  }
  sides.clear();
  sides.shrink_to_fit();
  src.clear();
  src.shrink_to_fit();
  delta_id_of.clear();

  // delta id of half-edge h and the sign to apply crossing it left to right
  auto side_of = [&](int64_t h, int32_t& delta_id, int& sign) {
    delta_id = half_delta[(std::size_t)h];
    sign = from(h) < to(h) ? -1 : 1;
  };

  // interned winding states, each stored once; state 0 is outside (all
  // zero). The index maps a state's hash to its ids.
  std::vector<Entries> states(1);
  std::unordered_multimap<uint64_t, int32_t> state_index;
  EntriesHash hasher;
  state_index.emplace((uint64_t)hasher(states[0]), 0);
  auto intern = [&](Entries&& e) -> int32_t {
    uint64_t h = (uint64_t)hasher(e);
    auto range = state_index.equal_range(h);
    for (auto it = range.first; it != range.second; ++it) {
      if (states[(std::size_t)it->second] == e) return it->second;
    }
    int32_t id = (int32_t)states.size();
    states.push_back(std::move(e));
    state_index.emplace(h, id);
    return id;
  };
  std::unordered_map<uint64_t, int32_t, U64Hash> cache;  // (state, delta, sign) -> state
  auto transit = [&](int32_t s, int32_t delta_id, int sign) -> int32_t {
    if (delta_id < 0) return s;
    uint64_t k = (uint64_t)(uint32_t)s << 32 | ((uint64_t)(uint32_t)delta_id << 1 | (sign > 0));
    auto c = cache.find(k);
    if (c != cache.end()) return c->second;
    int32_t id = intern(step(states[(std::size_t)s], deltas[(std::size_t)delta_id], sign));
    cache.emplace(k, id);
    return id;
  };

  Labels out;
  std::vector<int32_t> tri_state((std::size_t)nt, -1);
  std::vector<int64_t> queue;
  queue.reserve((std::size_t)nt);

  auto walk = [&](int64_t seed) {
    queue.clear();
    queue.push_back(seed);
    for (std::size_t q = 0; q < queue.size(); q++) {
      int64_t t = queue[q];
      int32_t s = tri_state[(std::size_t)t];
      for (int k = 0; k < 3; k++) {
        int64_t h = 3 * t + k;
        int64_t g = twin[(std::size_t)h];
        if (g < 0) continue;
        int32_t delta_id;
        int sign = 0;
        side_of(h, delta_id, sign);
        int32_t su = transit(s, delta_id, sign);
        int64_t u = g / 3;
        int32_t& known = tri_state[(std::size_t)u];
        if (known < 0) {
          known = su;
          queue.push_back(u);
        } else if (known != su && h < g) {
          out.inconsistent_edges++;
        }
      }
    }
  };

  // seed each component from a boundary side: stepping from outside (on
  // the right of the side) into its triangle; check every other boundary side
  for (int64_t h = 0; h < nh; h++) {
    if (twin[(std::size_t)h] >= 0) continue;
    int64_t t = h / 3;
    int32_t delta_id;
    int sign = 0;
    side_of(h, delta_id, sign);
    int32_t st = transit(0, delta_id, -sign);
    if (tri_state[(std::size_t)t] < 0) {
      tri_state[(std::size_t)t] = st;
      walk(t);
    } else if (tri_state[(std::size_t)t] != st) {
      out.inconsistent_edges++;
    }
  }
  // a component with no boundary side cannot occur in a finite planar
  // mesh; label any such triangles from w = 0 rather than leave them out
  for (int64_t t = 0; t < nt; t++) {
    if (tri_state[(std::size_t)t] < 0) {
      tri_state[(std::size_t)t] = 0;
      walk(t);
    }
  }
  out.n_state = (int64_t)states.size();

  // covering sets under the rule, numbered by first appearance
  std::vector<int32_t> state_set(states.size(), -2);
  std::unordered_map<std::vector<int32_t>, int32_t, IdsHash> set_id;
  out.set_offset.push_back(0);
  out.tri_set.assign((std::size_t)nt, -1);
  for (int64_t t = 0; t < nt; t++) {
    int32_t s = tri_state[(std::size_t)t];
    if (state_set[(std::size_t)s] == -2) {
      std::vector<int32_t> f;
      for (const Entry& x : states[(std::size_t)s]) {
        if (covers(x.second, rule)) f.push_back(x.first);
      }
      if (f.empty()) {
        state_set[(std::size_t)s] = -1;
      } else {
        auto ins = set_id.emplace(f, (int32_t)(out.set_offset.size() - 1));
        if (ins.second) {
          out.set_feature.insert(out.set_feature.end(), f.begin(), f.end());
          out.set_offset.push_back((int64_t)out.set_feature.size());
        }
        state_set[(std::size_t)s] = ins.first->second;
      }
    }
    out.tri_set[(std::size_t)t] = state_set[(std::size_t)s];
  }
  return out;
}

}  // namespace overlay
}  // namespace polymer

#endif  // POLYMER_OVERLAY_HPP

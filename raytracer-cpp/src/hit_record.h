#pragma once

#include "vec3.h"
#include "material.h"

class HitRecord {
    public:
        bool did_hit;
        float t;
        Vec3 position, normal;
        Material *material;
        Vec2 texture_coord;
};

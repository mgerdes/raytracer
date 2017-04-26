#include "ray.h"

Vec3 Ray::point_at_time(float t) const {
    return origin + t * direction;
}
